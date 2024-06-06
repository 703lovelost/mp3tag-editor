module Converter (
    readID3Header,
    readID3Data,
    convertToBinary,
    writeID3Data,
    recreateMP3File
    ) where
    import ID3Reader
    import ID3TagList
    import System.IO
    import System.Directory
    import qualified Data.Map as M
    import qualified Data.Map.Strict as MS
    import Data.List.Split
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as BSL
    import qualified Data.ByteString.Char8 as BSC8

    type ID3Catalog = M.Map String String

    -- Бинарное чтение MP3-файла, упаковка в байтовую строку.
    readBinaryFile :: FilePath -> IO BS.ByteString
    readBinaryFile file = BS.readFile file

    -- Запись байтовой строки содержимого MP3-файла в бинарный файл.
    -- Позволяет не сломать программу при перемещении MP3-файла из искомой директории.
    writeBinaryFile :: FilePath -> BS.ByteString -> IO ()
    writeBinaryFile file content = BS.writeFile file content

    -- Связующая функция с выводом лога о генерации бинарного temp-файла.
    convertToBinary :: FilePath -> IO ()
    convertToBinary inputFile = do
        content <- readBinaryFile inputFile
        writeBinaryFile "temp_audio.bin" content
        -- putStrLn "Temporary binary file is generated."

    -- Чтение ID3-заголовка из temp-файла.
    readID3Header :: IO BSC8.ByteString
    readID3Header = do
        tempCreated <- doesFileExist "temp_audio.bin"

        if tempCreated then do -- (1.1)
                -- (1.1.1)
                handle <- openBinaryFile "temp_audio.bin" ReadMode
                content <- BS.hGet handle 10
                hClose handle

                -- (1.1.2)
                let mp3tag = BSC8.take 3 content
                let id3v2tag = BSC8.pack "ID3" -- Заголовок ID3v.2
                let id3v1tag = BSC8.pack "TAG" -- Заголовок ID3v.1

                if mp3tag == id3v2tag
                    then do
                        let flags = BSC8.take 1 $ BSC8.drop 5 content

                        -- (1.1.2.1)
                        if flags == BSC8.pack "\NUL"
                            then return content
                            else return $ BSC8.pack "Flagged data is not supported." -- TODO Error Message.
                    else if mp3tag == id3v1tag
                        then return $ BSC8.pack "ID3v1 is not supported." -- TODO Error Message.
                        else return $ BSC8.pack "Invalid ID3 data." -- TODO Error Message.
            else error "Binary file generation error." -- (1.2) -- TODO Error Message.

    -- Чтение всего массива ID3-метаданных. Вызывается после вычисления нужного размера (getDataSize в ID3Reader).
    -- (*) Заголовок метаданных выбрасываем.
    readID3Data :: Int -> IO BSC8.ByteString
    readID3Data size = do
        tempCreated <- doesFileExist "temp_audio.bin"

        if tempCreated
            then do
                handle <- openBinaryFile "temp_audio.bin" ReadMode
                content <- BS.hGet handle size
                hClose handle

                BS.writeFile "temp_audio_old.bin" content

                return (BSC8.drop 10 content) -- (*)
            else error "Binary file generation error." -- TODO Error Message.

    tagToString :: (String, String) -> String
    tagToString (key, value) =  if M.member key tagList
                                    then do
                                        let size = generateStringSize $ (length value) + 1
                                        key ++ size ++ "\NUL\NUL" ++ "\03" ++ value
                                    else value

    addTagsToFile :: FilePath -> ID3Catalog -> IO ()
    addTagsToFile file catalog = do
        let catalogToStringArray = map tagToString (M.toList catalog)
        let stringArrayToString = BSC8.pack $ concat catalogToStringArray

        handle <- openBinaryFile file AppendMode
        BSC8.hPutStr handle stringArrayToString
        hClose handle

    writeID3Data :: String -> IO ()
    writeID3Data version = do
        content <- readFile "temp_catalog"
        let catalog = read content :: ID3Catalog

        let size = M.lookup "SIZE" catalog

        case size of
            Just num -> do
                let sizeBytes = generateStringSize (read num)
                let header = "ID3" ++ version ++ "\NUL\NUL" ++ sizeBytes

                writeBinaryFile "temp_audio_new.bin" $ BSC8.pack header
                addTagsToFile "temp_audio_new.bin" $ M.delete "SIZE" catalog

                content <- readBinaryFile "temp_audio_new.bin"
                let sizeLeft = (read num) - (BSC8.length content)

                handle <- openBinaryFile "temp_audio_new.bin" AppendMode
                BSC8.hPutStr handle $ BSC8.pack $ replicate sizeLeft '\NUL'
                hClose handle
            Nothing -> do
                let sizeBytes = generateStringSize 2000
                let header = "ID3" ++ version ++ "\NUL\NUL" ++ sizeBytes

                writeBinaryFile "temp_audio_new.bin" $ BSC8.pack header
                addTagsToFile "temp_audio_new.bin" $ M.delete "SIZE" catalog

                content <- readBinaryFile "temp_audio_new.bin"
                let sizeLeft = 2000 - (BSC8.length content)

                handle <- openBinaryFile "temp_audio_new.bin" AppendMode
                BSC8.hPutStr handle $ BSC8.pack $ replicate sizeLeft '\NUL'
                hClose handle

    recreateMP3File :: IO ()
    recreateMP3File = do
        contentMP3 <- BSC8.readFile "temp_audio.bin"
        contentOldMeta <- BSC8.readFile "temp_audio_old.bin"
        contentNewMeta <- BSC8.readFile "temp_audio_new.bin"
        let metaSize = BSC8.length contentOldMeta

        putStrLn $ BSC8.unpack contentOldMeta
        putStrLn $ BSC8.unpack contentNewMeta     

        let noMeta = BSC8.drop metaSize contentMP3

        BSC8.writeFile "result.mp3" $ BSC8.append contentNewMeta noMeta





