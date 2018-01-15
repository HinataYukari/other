-- 全ての目が一度出るまでに振らなければならないサイコロの回数の期待値
import System.Random
import Data.List

type Tryal = Int    --試行回数
type Eye   = Int    --出目
type Seed  = Int    --seed
ts = 100000         --検証回数
seeds = [1..ts]     --seedのリスト

dice :: Tryal -> Seed -> [Eye]
dice n seed = take n $ map (succ . flip mod 6) $ randoms (mkStdGen seed)

judge :: [Eye] -> Bool
judge results = and [eye `elem` results| eye <- [1..6]]

solve :: Seed -> Tryal
solve s = succ . length $ takeWhile (not . judge) [dice k s | k <- [1..]]

answer :: Double
answer = (/ (fromIntegral ts)) . fromIntegral . sum $ map solve seeds

trueAnswer :: Double
trueAnswer = sum [6.0 / fromIntegral k | k <- [1..6]]

main :: IO ()
main = do
    --n回ふってでた目(引数はnとseed)
    print $ dice 10 0
    -- output:[4,4,6,3,5,4,5,2,3,3]

    -- 全ての目が少なくとも一度出るまでに振らなければならないサイコロの回数(引数はseed)
    print $ map solve [1..10]
    -- output:[7,10,15,8,15,14,22,15,18,20]

    -- 全ての目が少なくとも一度出るまでに振らなければならないサイコロの回数の平均値
    print answer
    -- output:14.70051

    -- 全ての目が少なくとも一度出るまでに振らなければならないサイコロの回数の期待値
    print trueAnswer
    -- output:14.7
