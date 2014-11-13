import Fast_PPM
import Data.List
--mport Data.List.Split
import Data.Array.Unboxed 
import Colour
import System

-- this is a haskell experiment to generate the buddhabrot fractal.
main = do
	args <- getArgs
	let
		resolution = read $ head args
		cutoff = read $ head $ tail args
	--	grid :: Array (Double, Double) Double
	--	grid = array ((1, 1),(width, height)) [((i,j), 0) | i <- [1..width], j <- [1..height]]
	save_ppm "buddhabrot.ppm" (buddhabrot resolution cutoff)
--	print "done"
	return ()

buddhabrot resolution cutoff =
	toPPM width height $ map (generateBuddhaColor maxColor) (pixels)
	where
		maxDistance = 4.0
		width = fromIntegral $ round $ 3 * resolution
		height = fromIntegral $ round $ 2 * resolution
		xRange = map (\x -> x / resolution) [-2*resolution..resolution]
		yRange = map (\x -> x / resolution) [-resolution..resolution]
		comandelbrot = filter (\(cx,cy) -> not (isMandelbrot cutoff maxDistance 0 0 0 cx cy))  [(x,y) | y <- yRange, x <- xRange]
		inBuddhaBounds x y = -2 < x && x < 1 && -1 < y && y < 1
		buddhabrot_p :: Array (Int, Int) Int
		buddhabrot_p = accumArray (+) 0 ((round (-2*resolution),round (-resolution)),(round resolution, round resolution)) [((round (x * resolution), round (y * resolution)),1) | (x,y) <- (concatMap (\(a,b) -> mandelbrotPoints cutoff 0 0 0 a b) comandelbrot), inBuddhaBounds x y]
		pixels = elems buddhabrot_p
		maxColor = fromIntegral $ maximum pixels  


toPPM width height pixels =
	splitEvery (height + 1) pixels

splitEvery w [] = []
splitEvery w list = (take w list) : (splitEvery w (drop w list))

isMandelbrot :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
isMandelbrot cutoff maxDistance n x y cx cy 
	| n < cutoff && abs < maxDistance = isMandelbrot cutoff maxDistance (n+1) nx ny cx cy
	| otherwise = n == cutoff
	where abs = nx*nx + ny*ny
	      nx = x*x - y*y + cx
	      ny = 2*x*y + cy

-- calculates all the points the mandelbrot iteration crosses with these source parameters
-- for a specific cutoff
mandelbrotPoints :: Double -> Double -> Double -> Double -> Double -> Double -> [(Double, Double)]
mandelbrotPoints cutoff n x y cx cy 
	| n < cutoff = (nx, ny) : mandelbrotPoints cutoff (n + 1) nx ny cx cy
	| otherwise = []
	where nx = x*x - y*y + cx
	      ny = 2*x*y + cy

generateBuddhaColor :: Double -> Int -> Colour
generateBuddhaColor maxColor int = 
	Colour r g b
	where 
		r = a
		g = a
		b = a
		a = fromIntegral int / maxColor
