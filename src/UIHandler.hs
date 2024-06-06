module UIHandler (
    renderEditingForm
    ) where
    import Converter 
    import ID3Reader
    import ID3TagList
    import qualified Data.Map as M
    import Control.Monad
    import Control.Monad.IO.Class
    import Data.IORef
    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.Builder
    import qualified Data.List as List
    import Data.List.Split
    import Data.Char
    import Data.Encoding
    import Data.Encoding.CP1251
    import qualified Data.ByteString.Char8 as BSC8 hiding (uncons, unsnoc)

    type ID3Catalog = M.Map String String

    initFileLoad :: FileChooserClass self => self -> IO BSC8.ByteString
    initFileLoad fileChooserDialog = do
        filename <- fileChooserGetFilename fileChooserDialog
        case filename of
            Just filepath   -> do 
                convertToBinary filepath
                header <- readID3Header
                return header
            Nothing         -> error "Poop shit." -- TODO Error message.

    loadData :: BSC8.ByteString -> IO (ID3Catalog)
    loadData id3header = do
        let dataSize = getDataSize 1 id3header
        id3data <- readID3Data dataSize

        createCatalog dataSize id3data
        content <- readFile "temp_catalog"
        let catalog = read content

        return catalog
        -- print id3data

    renderTextValue :: (LabelClass label, EntryClass entry) => label -> entry -> ID3Catalog -> String -> IO ()
    renderTextValue label entry catalog tag = do
        let decode value = decodeString CP1251 value

        let labelText = M.lookup tag tagList
        case labelText of
            Just text -> labelSetText label $ decode text
            Nothing -> labelSetText label ""

        let entryText = M.lookup tag catalog
        case entryText of
            Just text -> entrySetText entry $ decode text
            Nothing -> entrySetText entry ""

    renderNumericValues :: (LabelClass label, EntryClass entry) => label -> entry -> entry -> ID3Catalog -> String -> IO ()
    renderNumericValues label entryNum entryAmount catalog tag = do
        let decode value = decodeString CP1251 value

        let labelText = M.lookup tag tagList
        case labelText of
            Just text -> labelSetText label $ decode text
            Nothing -> labelSetText label ""
        
        let trackText = M.lookup tag catalog
        case trackText of
            Just text -> if elem '/' text
                then do
                    let number = List.uncons $ splitOn "/" text
                    case number of
                        Just (num, _) -> entrySetText entryNum $ decode num
                        Nothing -> entrySetText entryNum $ decode ""

                    let amount = last $ splitOn "/" text
                    entrySetText entryAmount $ decode amount
                else do
                    entrySetText entryNum $ decode text
                    entrySetText entryAmount $ decode ""                                
            Nothing -> do
                entrySetText entryNum $ decode ""
                entrySetText entryAmount $ decode ""       

    writeTextToCatalog :: (EntryClass entry) => entry -> String -> String -> ID3Catalog -> ID3Catalog
    writeTextToCatalog entry tag entryText catalog = do
        let encode value = encodeString CP1251 value
        let tagIsPresent = M.member tag catalog

        if tagIsPresent || not (null entryText)
            then M.insert tag (encode entryText) catalog
            else catalog

    writeNumericToCatalog :: (EntryClass entry) => entry -> entry -> String -> String -> String -> ID3Catalog -> ID3Catalog
    writeNumericToCatalog entryNum entryAmount tag entryTextNum entryTextAmount catalog = do
        let encode value = encodeString CP1251 value
        let tagIsPresent = M.member tag catalog

        if tagIsPresent
            then if (null entryTextNum && null entryTextAmount)
                then M.insert tag "" catalog
                else if (not (null entryTextNum) && null entryTextAmount)
                    then M.insert tag (encode entryTextNum) catalog
                    else if (null entryTextNum && not (null entryTextAmount))
                        then M.insert tag (encode ("0/" ++ entryTextAmount)) catalog
                        else M.insert tag (encode (entryTextNum ++ "/" ++ entryTextAmount)) catalog
            else catalog

    validateNumValue :: String -> String
    validateNumValue value = if all isDigit value then value else ""

    renderEditingForm :: IO ()
    renderEditingForm = do
        void initGUI

        -- Инициализируем Glade-объекты, чтобы связать существующий интерфейс с бэкендом.
        builder <- builderNew
        builderAddFromFile builder "src/glade-src/mp3tag-editor-gui.glade"

        let build cast name = builderGetObject builder cast name

        window <- build castToWindow "mainWindow"
        mainBox <- build castToBox "mainBox"
        saveBtn <- build castToButton "saveBtn"
        infoLabel <- build castToLabel "infoLabel"

        openBtn <- build castToFileChooserButton "openBtn"
        fileChooserDialog <- build castToFileChooserDialog "fileChooserDialog"
        filePathLabel <- build castToLabel "filePathLabel"

        dataGrid <- build castToGrid "dataGrid"
        labelVersion <- build castToLabel "labelVersion"
        labelTitle <- build castToLabel "labelTitle"
        labelArtist <- build castToLabel "labelArtist"
        labelAlbum <- build castToLabel "labelAlbum"
        labelGenre <- build castToLabel "labelGenre"
        labelTrack <- build castToLabel "labelTrack"
        labelComposer <- build castToLabel "labelComposer"
        labelBPM <- build castToLabel "labelBPM"
        entryTitle <- build castToEntry "entryTitle"
        entryArtist <- build castToEntry "entryArtist"
        entryAlbum <- build castToEntry "entryAlbum"
        entryGenre <- build castToEntry "entryGenre"
        entryTrackSingle <- build castToEntry "entryTrackSingle"
        entryTrackAmount <- build castToEntry "entryTrackAmount"
        entryComposer <- build castToEntry "entryComposer"
        entryBPM <- build castToEntry "entryBPM"

        -- Обработка события нажатия на файл, чтобы отображать его путь в окне выбора.
        fileChooserDialog `on` fileSelectionChanged $ do 
            filename <- fileChooserGetFilename fileChooserDialog
            case filename of
                Just filepath   -> do 
                    set filePathLabel [ labelLabel := filepath ]
                Nothing         -> do
                    set filePathLabel [ labelLabel := "Choose the MP3-file." ]

        -- Обработка события выбора файла.
        fileChooserDialog `on` fileActivated $ do
            widgetHide fileChooserDialog
            id3header <- initFileLoad fileChooserDialog

            catalog <- loadData id3header

            let version = getVersion id3header
            labelSetLabel labelVersion $ "Version: " ++ version

            let decode value = decodeString CP1251 value
            renderTextValue labelTitle entryTitle catalog "TIT2"
            renderTextValue labelArtist entryArtist catalog "TPE1"
            renderTextValue labelAlbum entryAlbum catalog "TALB"
            renderTextValue labelGenre entryGenre catalog "TCON"
            renderNumericValues labelTrack entryTrackSingle entryTrackAmount catalog "TRCK"
            renderTextValue labelComposer entryComposer catalog "TCOM"
            renderTextValue labelBPM entryBPM catalog "TBPM"                

            widgetShowAll window

        -- Обработка события нажатия кнопки сохранения.
        saveBtn `on` buttonActivated $ do
            id3header <- initFileLoad fileChooserDialog
            catalog <- loadData id3header

            entryTitleText <- entryGetText entryTitle
            let catalogAddedTitle = writeTextToCatalog entryTitle "TIT2" entryTitleText catalog

            entryArtistText <- entryGetText entryArtist
            let catalogAddedArtist = writeTextToCatalog entryArtist "TPE1" entryArtistText catalogAddedTitle

            entryAlbumText <- entryGetText entryAlbum
            let catalogAddedAlbum = writeTextToCatalog entryAlbum "TALB" entryAlbumText catalogAddedArtist

            entryGenreText <- entryGetText entryGenre
            let catalogAddedGenre = writeTextToCatalog entryGenre "TCON" entryGenreText catalogAddedAlbum

            entryComposerText <- entryGetText entryComposer
            let catalogAddedComposer = writeTextToCatalog entryComposer "TCOM" entryComposerText catalogAddedGenre

            entryBPMText <- entryGetText entryBPM
            let catalogAddedBPM = writeTextToCatalog entryBPM "TBPM" (validateNumValue entryBPMText) catalogAddedComposer

            entryTrackNumText <- entryGetText entryTrackSingle
            entryTrackAmountText <- entryGetText entryTrackAmount
            let catalogAddedTrack = writeNumericToCatalog entryTrackSingle entryTrackAmount "TRCK" (validateNumValue entryTrackNumText) (validateNumValue entryTrackAmountText) catalogAddedBPM

            overwriteData catalogAddedTrack

            version <- labelGetText labelVersion
            if (last version) == '3'
                then writeID3Data "\03"
                else writeID3Data "\04"

            recreateMP3File

        -- Обработка события закрытия главного окна - убиваем процесс.
        window `on` deleteEvent $ do
            liftIO mainQuit
            return False
        
        widgetShowAll window
        widgetHide saveBtn
        widgetHide dataGrid
        mainGUI
