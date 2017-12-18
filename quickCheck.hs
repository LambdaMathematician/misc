import Test.QuickCheck

colorToBV :: String -> [Int]
colorToBV color
    | color == "black"   = [0,0,0]
    | color == "red"     = [1,0,0]
    | color == "green"   = [0,1,0]
    | color == "blue"    = [0,0,1]
    | color == "magenta" = [1,0,1]
    | color == "cyan"    = [0,1,1]
    | color == "yellow"  = [1,1,0]
    | color == "white"   = [1,1,1]
    | otherwise          = error "Please choose from black, red, green, blue, magenta, teal, yellow, or white."

isSchoolWeek = True
isNotSchoolWeek = False

allowedWorkHours :: Int -> Bool -> Int
allowedWorkHours age schoolweek
    | age < 14  = 0
    | age < 16 && schoolweek = 18
    | age < 16 && not schoolweek = 40
    | age == 25 = 20
    | otherwise = 168

test1 = allowedWorkHours (-8) isSchoolWeek    == 0
test2 = allowedWorkHours  13  isSchoolWeek    == 0
test3 = allowedWorkHours  14  isSchoolWeek    == 18
test4 = allowedWorkHours  14  isNotSchoolWeek == 40
test5 = allowedWorkHours  15  isSchoolWeek    == 18
test6 = allowedWorkHours  15  isNotSchoolWeek == 40
test7 = allowedWorkHours  16  isNotSchoolWeek == 168
test8 = allowedWorkHours  83  isSchoolWeek    == 168

eqc1 = [test1, test2]
eqc2 = [test3, test4]
eqc3 = [test5, test6]
eqc4 = [test7, test8]

allTestsPassed = and $ eqc1 ++ eqc2 ++ eqc3 ++ eqc4

prop_validOutputs age schoolweek = elem (allowedWorkHours age schoolweek) [0,18,40,168]
    where
        types = (age::Int, schoolweek :: Bool)

prop_revrev xs = reverse (reverse xs) == xs

prop_permit age schoolweek = age == 14 || age == 15 ==> elem (allowedWorkHours age schoolweek) [18,40]

prop_adult age schoolweek = age > 16 ==> allowedWorkHours age schoolweek == 168

prop_child age schoolweek = age < 14 ==> allowedWorkHours age schoolweek == 0


prop_reverse_reverse :: [Int] -> Property
prop_reverse_reverse xs =
  label ("length of input is " ++ show (length xs)) $
    reverse (reverse xs) === xs


prop_reverse_reverse' :: [Int] -> Property
prop_reverse_reverse' xs =
  label ("input length " ++ (isOver3 xs)) $
    reverse (reverse xs) === xs
    where
        isOver3 xs
            | length xs > 3 = "> 3."
            | otherwise = "<= 3."

prop_sqrt x = abs ((sqrt x)**2 - abs x) < 2.2e-17

prop_checkColors color =
    label ("color: " ++ color) (mytest color)
    where
        mytest col 
            | col == "black"   = colorToBV col == [0,0,0]
            | col == "red"     = colorToBV col == [1,0,0]
            | col == "green"   = colorToBV col == [0,1,0]
            | col == "blue"    = colorToBV col == [0,0,1]
            | col == "magenta" = colorToBV col == [1,0,1]
            | col == "cyan"    = colorToBV col == [0,1,1]
            | col == "yellow"  = colorToBV col == [1,1,0]
            | col == "white"   = colorToBV col == [1,1,1]

myGen = elements ["black", "red", "green", "blue", "magenta", "cyan", "yellow", "white"]