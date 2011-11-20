import Test.HUnit
import Math

main = runTestTT allTests

allTests = TestList [matrixTests, transMatrixTests, rotMatrixTests]

matrixTests = "matrixTests" ~: TestList
    [swap12 .*. swap12 ~?= m4id
    ,swap12 .*. swap23 ~?= s12s23
    ,swap23 .*. swap12 ~?= s23s12
    ,transpose swap12 ~?= swap12
    ,transpose s12s23 ~?= s23s12
    ,nums .*. nums ~?= numsSq
    ]
    
transMatrixTests = "transMatrixTests" ~: TestList
    [(transM4 (vec4 f3e2)) .*. (pt4 f3e1) ~?= pt4 (f3e1 .+. f3e2)
    ,(transM4 (vec4 f3e2)) .*. (pt4 f3e2) ~?= pt4 (2 *. f3e2)
    ,(transM4 (vec4 f3e2)) .*. (pt4 v123) ~?= pt4 (v123 .+. f3e2)
    ]

rotMatrixTests = "rotMatrixTests" ~: TestList
    [(rotM4 f3e3 90) .*. (f4e1) ~?= f4e2
    ,(rotM4 f3e3 (-90)) .*. (f4e2) ~?= f4e1
    ,(rotM4 f3e3 180) .*. (f4e1) ~?= (-1) *. f4e1
    ,(rotM4 v123 42) .*. (vec4 v123) ~?= vec4 v123
    ,(rotM4 v123 42) .*. (pt4 v123) ~?= pt4 v123
    ,(rotM4 v123 42) .*. (rotM4 v123 (-42)) ~?= m4id
    ,(rotM4 v123 42) .*. (transpose (rotM4 v123 (42))) ~?= m4id
    ,(transpose (rotM4 v123 (42))) ~?= (rotM4 v123 (-42))
    ,(rotM4 v123 10) .*. (rotM4 v123 20) ~?= (rotM4 v123 20) .*. (rotM4 v123 10)
    ,(rotM4 v123 10) .*. (rotM4 v123 20) ~?= rotM4 v123 30
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
