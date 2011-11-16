import Haray
import OutputSDL
import OutputPPM

main = do
--	renderSDL (PerLines 2) testImage
	renderPPM "out.ppm" testImage
