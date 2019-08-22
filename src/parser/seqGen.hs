main :: IO ()
main = putStrLn (implMulti 21)

implMultis :: Int -> String
implMultis 0 = ""
implMultis n = implMultis (n - 1) ++ implMulti n

implMulti :: Int -> String
implMulti n = "impl_multi!(\n    " ++ params (n - 1) ++ ");\n"

params :: Int -> String
params 0 = param 0
params n = params (n - 1) ++ ";\n    " ++ param n

param :: Int -> String
param n = "P" ++ sn ++ ", R" ++ sn ++ ", p" ++ sn ++ ", r" ++ sn
    where
        sn = show n