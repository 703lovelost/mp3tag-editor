module ID3TagList (
    tagList
    ) where
    import qualified Data.Map as M

    -- Данный модуль содержит мапу с тегами и их контекстными значениями
	-- Названия даны в соответствии со списком для ExifTool - см. https://exiftool.org/TagNames/ID3.html.
	-- В мапе присутствуют только те теги, которые будут отображаться на фронтенде.

    tagList :: M.Map String String
    tagList = M.fromList [
		("TALB", "Album"),
		("TBPM", "BPM"),
		("TCOM", "Composer"),
		("TCON", "Genre"),
		("TIT2", "Title"),
		("TPE1", "Artist"),
		("TPOS", "Disc number"),
		("TRCK", "Track") ]