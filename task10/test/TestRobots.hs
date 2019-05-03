import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        greg = robot "Greg" 25 100
        dead = robot "A" 50 0
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        
        , testCase "Test for getAttack" $
            getAttack walter @?= 50
        
        , testCase "Test for getHealth" $
            getHealth walter @?= 50
        
        , testCase "Test for setName" $
            setName "A" walter @?= ("A", 50, 50)
        
        , testCase "Test for setAttack" $
            setAttack 100 walter @?= ("Walter", 100, 50)
        
        , testCase "Test for setHealth" $
            setHealth 100 walter @?= ("Walter", 50, 100)
        
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        
        , testCase "Test for damage" $
            damage walter 100 @?= ("Walter", 50, -50)
        
        , testCase "Test for isAlive, alive robot" $
            isAlive walter @?= True
        
        , testCase "Test for isAlive, dead robot" $
            isAlive dead @?= False
        
        , testCase "Test for fight, alive attacker" $
            fight greg walter @?= setHealth 25 walter
        
        , testCase "Test for fight, dead attacker" $
            fight dead walter @?= walter
        
        , testCase "Test for threeRoundFight" $
            threeRoundFight greg walter @?= ("Greg", 25, 50)
        
        , testCase "Test for neueRobotAttak" $
            neueRobotAttak walter @?= ("Walter", 50, 20)
        
        , testCase "Test for survivors" $
            survivors @?= [("Kek", 3, 20)]
        ]
