{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeFamilies #-}
import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.IO.Class
import Data.Ix (inRange)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import MusicBrainz (Context, MusicBrainz, mbDb, runMb, runMbContext, withTransactionRollBack)
import MusicBrainz.Types
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor
import Test.SmallCheck
import Test.SmallCheck.Series

import qualified Data.Text as T

instance Monad m => Serial m PartialDate where
  series = (\y m d -> (y, m, d) ^?! partialDate)
             <$> series
             <*> (fmap (succ . (`mod` 12)) <$> series)
             <*> (fmap (succ . (`mod` 31)) <$> series)

instance Monad m => Serial m T.Text where
  series = newtypeCons T.pack

instance Monad m => Serial m Artist where
  series = decDepth $ Artist <$> pure "Artist name"
                             <~> pure "Artist sortname"
                             <~> pure "Comment"
                             <~> series
                             <~> series
                             <~> series
                             <~> pure Nothing
                             <~> pure Nothing
                             <~> pure Nothing

instance Monad m => Serial m Editor where
  series = decDepth $ Editor <$> series <~> series

creatingWorks :: Context -> Connection -> Property IO
creatingWorks c mirrorConn =
  exists $ \editor ->
  forAll $ \artist ->
  mb c $ do
    e <- entityRef <$> register editor
    edit <- openEdit
    newArtistId <-  withEdit edit $
      create e ArtistTree { artistData = artist
                          , artistRelationships = mempty
                          , artistAliases = mempty
                          , artistIpiCodes = mempty
                          , artistAnnotation = mempty
                          } >>= viewRevision

    liftIO $ begin mirrorConn *>
      mirror c mirrorConn (coreRef newArtistId)

    Just x@( (gid, name, sortName, bYear, bMonth)
        :. (bDay, eYear, eMonth, eDay, typeId)
        :. (countryId, genderId, comment)) <- liftIO $ listToMaybe <$> query mirrorConn
      [sql|
        SELECT
          gid, name.name, sort_name.name,
          begin_date_year, begin_date_month, begin_date_day,
          end_date_year, end_date_month, end_date_day,
          type, country, gender, comment
        FROM artist
        JOIN artist_name name ON (name.id = artist.name)
        JOIN artist_name sort_name ON (sort_name.id = artist.sort_name)
        WHERE gid = ?
      |] (Only $ coreRef newArtistId)

    liftIO $ rollback mirrorConn

    -- liftIO $ print x
    liftIO $ print artist

    let conditions =
                 [ gid == coreRef newArtistId
                 , name == artistName artist
                 , sortName == artistSortName artist
                 , bYear == dateYear (artistBeginDate artist)
                 , bMonth == dateMonth (artistBeginDate artist)
                 , bDay == dateDay (artistBeginDate artist)
                 , eYear == dateYear (artistEndDate artist)
                 , eMonth == dateMonth (artistEndDate artist)
                 , eDay == dateDay (artistEndDate artist)
                 , typeId == artistType artist
                 , countryId == artistCountry artist
                 , genderId == artistGender artist
                 , comment == artistComment artist
                 ]

    return $ and conditions

mirror :: Context -> Connection -> Ref Artist -> IO ()
mirror mb c artistRef = do
  artist <- coreData <$> runMbContext mb (findLatest artistRef)

  nameId <- resolveArtistName (artistName artist)
  sortNameId <- resolveArtistName (artistSortName artist)

  void $ execute c
    [sql|
      INSERT INTO artist (gid, name, sort_name, begin_date_year, begin_date_month, begin_date_day,
                          end_date_year, end_date_month, end_date_day, ended, comment)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |] $
    ( artistRef, nameId :: Int, sortNameId :: Int
    , dateYear (artistBeginDate artist), dateMonth (artistBeginDate artist), dateDay (artistBeginDate artist)
    , dateYear (artistEndDate artist), dateMonth (artistEndDate artist), dateDay (artistEndDate artist) ) :.
    ( artistEnded artist, artistComment artist
    )
  where
    resolveArtistName n = do
      id' <- fmap fromOnly . listToMaybe <$> query c [sql| SELECT id FROM artist_name WHERE name = ? |] (Only n)
      maybe (fromOnly . head <$> query c [sql| INSERT INTO artist_name (name) VALUES (?) RETURNING id |] (Only n)) return id'

mb :: Testable IO a => Context -> MusicBrainz a -> Property IO
mb context = monadic . runMbContext context . withTransactionRollBack
