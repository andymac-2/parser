main :: IO ()
main = putStrLn (implMulti 21)

implMultis :: Int -> String
implMultis 0 = ""
implMultis n = implMultis (n - 1) ++ implMulti n ++ implBitOr n

implMulti :: Int -> String
implMulti n = "impl_multi!( Alt" ++ sn ++ ";\n    " ++ params (n - 1) ++ ");\n"
    where
        sn = show n

implBitOr :: Int -> String
implBitOr n = "impl_bitor!( Alt" ++ sn ++ ", Alt" ++ show (n + 1) ++ ";\n    " ++ params (n - 1) ++ ");\n"
    where
        sn = show n

params :: Int -> String
params 0 = param 0
params n = params (n - 1) ++ ";\n    " ++ param n

param :: Int -> String
param n = "P" ++ sn ++ ", p" ++ sn
    where
        sn = show n