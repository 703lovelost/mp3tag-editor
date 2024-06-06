module ID3Reader (
	getVersion,
	getDataSize,
	createCatalog,
	overwriteData,
	generateStringSize) where
	import qualified ID3TagList as TL
	import Data.List
	import Data.Word
	import Data.Char
	import Data.Bits
	import System.IO
	import System.Directory
	import qualified Data.Map as M
	import Codec.Binary.UTF8.Light -- Для преобразования Char в Word.
	import qualified Data.ByteString.Char8 as BSC8

	type ID3Catalog = M.Map String String
	type Header = Int -- "1" - для ID3-заголовка, "2" - для заголовка фрейма.

	{- Блок определения версии на основе байтовой строки ID3-заголовка. -}

	getVersion :: BSC8.ByteString -> String
	getVersion id3header = do
		let version = BSC8.take 1 $ BSC8.drop 3 id3header
		let v2_3tag = BSC8.pack "\ETX"
		let v2_4tag = BSC8.pack "\EOT"

		if version == v2_3tag
			then "2.3"
			else if version == v2_4tag
				then "2.4"
				else "This version of ID3 is not supported." -- TODO Error message.

	{- Блок считывания байтовой строки с ID3-метаданными. -}

	-- Преобразование байтовой строки с данными о памяти в десятичный вид.
	-- Последовательность: BSC8.ByteString -> [Char] -> [Word32]
	byteStringToW32Array :: BSC8.ByteString -> [Word32]
	byteStringToW32Array array = map c2w $ BSC8.unpack $ array

	-- Исключение 7го бита из каждого значения.
	-- Пояснение: каждый байт с информацией о памяти имеет длину в 7 бит (%0xxxxxxx).
	excludeBitFromEachElem :: [Word32] -> [Word32]
	excludeBitFromEachElem array = map (\ a -> if a < 128 then a else a - 128) array

	-- Разворот массива байтов с имеющейся памятью, сдвиги для дальнейшего суммирования.
	orderBits :: [Word32] -> [Word32]
	orderBits array = zipWith (\ a b -> shift a (7 * b)) array [0..]

	-- Адаптация fromIntegral на необходимые типы данных.
	w32toInt :: Word32 -> Int
	w32toInt = fromIntegral

	-- Связующая функция получения байтовой строки.
	-- 1. Для ID3-заголовка:
	-- 1.1. В случае с заголовком метаданных - отбрасываем "ID3", байт с версией, байт с сабверсией, байт с флагами.
	-- 1.2. Преобразовываем получившуюся байтовую строку в массив чисел с предварительной информацией о размере массива метаданных.
	-- 1.3. Складываем значения массива, преобразовываем результат в Int для дальнейшего чтения из temp-файла.
	-- 1.4. Добавляем длину хэдера к итоговому результату.
	-- 2. Для заголовка фрейма
	-- TODO
	getDataSize :: Header -> BSC8.ByteString -> Int
	getDataSize headerType header = do
		case headerType of
			1 -> do -- (1)
				let sizeInfo = BSC8.drop 6 header -- (1.1)
				-- (1.2)
				let byteArrayWord32 = byteStringToW32Array sizeInfo
				let byteArrayBitExcluded = excludeBitFromEachElem byteArrayWord32
				let byteArrayBitsOrdered = orderBits $ reverse byteArrayBitExcluded

				let dataSize = w32toInt $ sum byteArrayBitsOrdered -- (1.3)
				let headerSize = BSC8.length header -- (1.4)

				(+) headerSize dataSize
			2 -> do -- (2)
				let sizeInfo = BSC8.take 4 $ BSC8.drop 4 header

				let byteArrayWord32 = byteStringToW32Array sizeInfo
				let byteArrayBitExcluded = excludeBitFromEachElem byteArrayWord32
				let byteArrayBitsOrdered = orderBits $ reverse byteArrayBitExcluded

				let dataSize = w32toInt $ sum byteArrayBitsOrdered
				dataSize

	{- Блок считывания фреймов: их заголовков и основного содержимого. -}

	readFrameValue :: Int -> BSC8.ByteString -> String
	readFrameValue size id3data = BSC8.unpack $ BSC8.take size id3data

	addToCatalog :: ID3Catalog -> String -> String -> ID3Catalog
	addToCatalog catalog tag value = M.insert tag value catalog

	readFrames :: BSC8.ByteString -> ID3Catalog -> ID3Catalog
	readFrames id3data catalog = do
		let frameTag = BSC8.unpack $ BSC8.take 4 id3data
		let emptyField = "\NUL\NUL\NUL\NUL"

		if frameTag == emptyField
			then catalog
			else do
				let frameHeader = BSC8.take 10 id3data

				let valueSize = getDataSize 2 frameHeader
				let frameSize = (+) valueSize 10

				let frameValueFrontend = readFrameValue valueSize $ BSC8.drop 10 id3data
				let frameValueBackend = readFrameValue frameSize id3data
				let tagShouldBeShown = M.member frameTag TL.tagList
				let updatedData = BSC8.drop frameSize id3data

				if tagShouldBeShown
					then do
						let updatedCatalog = addToCatalog catalog frameTag (drop 1 frameValueFrontend)
						readFrames updatedData updatedCatalog
					else do
						let updatedCatalog = addToCatalog catalog frameTag frameValueBackend
						readFrames updatedData updatedCatalog

	createCatalog :: Int -> BSC8.ByteString -> IO ()
	createCatalog dataSize id3data = do
		let catalog = M.insert "SIZE" (show dataSize) M.empty
		let formedCatalog = readFrames id3data catalog

		handle <- openFile "temp_catalog" WriteMode
		hPutStrLn handle $ show formedCatalog
		hClose handle

	overwriteData :: ID3Catalog -> IO ()
	overwriteData catalog = do
		removeFile "temp_catalog"
		handle <- openFile "temp_catalog" WriteMode
		hPutStrLn handle $ show catalog
		hClose handle

{-
	defineID3Size :: ID3Catalog -> ID3Catalog -> ID3Catalog
	defineID3Size catalogPrev catalogNew = do
		let sizeMaybe = M.lookup "SIZE" catalogPrev
		case sizeMaybe of
			Just size -> do
				if M.size (catalogNew) == 1 && M.member "SIZE" catalogNew
					then 
			Nothing -> catalogPrev

			then catalogPrev
			else do
				M.
-}

	-- readFrame :: BSC8.ByteString -> [String]

	w32ArrayToString :: [Word32] -> String
	w32ArrayToString array = map w2c array

	unorderBits :: [Word32] -> [Word32]
	unorderBits array = zipWith (\ a b -> shift a (shiftAmount b)) array [0..]
							where shiftAmount num = 7 * num * (-1)

	generateWord32Array :: Word32 -> Word32 -> [Word32]
	generateWord32Array value power | power == 5 = []
									| otherwise = [sortedValue] ++ (generateWord32Array (value - sortedValue) (power + 1)) ++ []
											where sortedValue = value `mod` (2 ^ (7 * power))
		
	intToW32 :: Int -> Word32
	intToW32 = fromIntegral

	generateStringSize :: Int -> String
	generateStringSize size = do
		let sizeW32 = intToW32 size
		let arrayW32 = generateWord32Array sizeW32 1
		let arrayUnordered = unorderBits arrayW32
		
		w32ArrayToString $ reverse arrayUnordered
