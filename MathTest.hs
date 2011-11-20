import Test.HUnit
import Math

main = runTestTT allTests

allTests = TestList [matrixTests, transfoMatrixTests]

matrixTests = "matrixTests" ~: TestList
    [swap12 .*. swap12 ~?= m4id
    ,swap12 .*. swap23 ~?= s12s23
    ,swap23 .*. swap12 ~?= s23s12
    ,transpose swap12 ~?= swap12
    ,transpose s12s23 ~?= s23s12
    ,nums .*. nums ~?= numsSq
    ]
    
transfoMatrixTests = "transfoMatrixTests" ~: TestList
    [(transM (vec4 f3e2)) .*. (pt4 f3e1) ~?= pt4 (f3e1 .+. f3e2)
    ,(transM (vec4 f3e2)) .*. (pt4 f3e2) ~?= pt4 (2 *. f3e2)
    ,(transM (vec4 f3e2)) .*. (pt4 v123) ~?= pt4 (v123 .+. f3e2)
    ]

swap12 = matrFromList [f4e2, f4e1, f4e3, f4e4]
swap23 = matrFromList [f4e1, f4e3, f4e2, f4e4]
swap13 = matrFromList [f4e3, f4e2, f4e3, f4e4]

s12s23 = matrFromList [f4e3, f4e1, f4e2, f4e4]
s23s12 = matrFromList [f4e2, f4e3, f4e1, f4e4]

nums = matrFromLists [[ 1,  2,  3,   4]
                     ,[ 5,  6,  7,   8]
                     ,[ 9, 10, 11,  12]
                     ,[13, 14, 15,  16]] :: M4

numsSq = matrFromLists [[ 90, 100, 110, 120]
                       ,[202, 228, 254, 280]
                       ,[314, 356, 398, 440]
                       ,[426, 484, 542, 600]] :: M4

v123 = F3 1 2 3

-- vim: expandtab smarttab sw=4 ts=4
