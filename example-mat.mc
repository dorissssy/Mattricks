/* The GCD algorithm in MicroC */



function main(int x) gives int{


    mat3 = int(1, 3)[ [1,2,3] ];


    return x;
}


/* 

    mat2b = MAT int(3,1)[ [1], [1], [1] ];

    mat3 = int(1, 3)[ [1,2,3] ];
    mat4 = int(2, 3)[ [1,2,3], [1,2,3]];
    mat5 = int(3, 3)[ [1,2,3], [4,5,6], [7,8,9] ];
    mat5 = int(3, 4)[ [1,2,3,4,5,6,6,7,8], [4,5,6], [7,8,9] ];



    [          1         ]       , [1]

LBRAC mat_array_rule RBRAC COMMA LBRAC mat_array_rule RBRAC
(Mat [MatValue (MatLiteral $1)]) :: [  Mat [MatValue (MatLiteral $1)] ] 


    Mat [MatValue (MatLiteral $1)] = [1]
    [ [1] ]
    
    

| x y z |
| a b c |
| i j k |
 [ [1,2] ]
[

    [

        [
        1, 2, 3
        ], 
        [
        1, 2, 3
        ]
    ]
];


[ 
    [1],
    [1] 
]
LBRAC mat_rule RBRAC: =>> Mat [$2] 
[1], [1] 
mat_rule COMMA mat_rule 

Mat [mat_rule COMMA mat_rule ] 

Mat [
    Mat [MatValue (MatLiteral $2)]  COMMA
    Mat [MatValue (MatLiteral $2)] 
] 


Mat [Mat ( mat_rule::[mat_rule] ) ] 



[

    [

        [

            [
                1
            ], 
            [
                1
            ]
        ], 
        [
            1
        ]
    ]
];




[

   

        [
        1
        ], 
        [
        1
        ]
    
];

[

    [
    1, 
        [
        2, 
            [
            3, 
            ]
        ]
    ]
];

[

    [

        [
            1, 2
        ], 
        3
    ]
];


    mat3 = int(1,3)[ [1,2,3] ];


    mat4 = int(1,3)[ [1,2,3] ];
[

    [
        1,
        2,
        3
    ]
];
    mat3 = int(1, 3)[ ];
    mat3 = int(1, 3) [3,2,1];
    mat3 = int(3, 1)[[1], [1], [1]];
=========================================
int(1,3) [[1,2,3]];

vi = MatValue (MatLiteral $1);

Mat [Mat [v1, v2, v3]]


LBRAC mat_rule RBRAC
        ||
LBRAC mat_rule RBRAC
        ||
LITERAL COMMA mat_rule (v1::[v2, v3])
                ||
            LITERAL COMMA mat_rule (v2::[v3])
                            ||
                          LITERAL
              ||
LITERAL COMMA LITERAL COMMA LITERAL
[

    [
    1
    ]
];
[

    [

        [
            1
        ], 
        [
            1
        ]
    ]
];

[[1], [1], [1]]

LBRAC (Mat [$2]) 
    COMMA (Mat [$2]) 
    COMMA (Mat [$2]) 
RBRAC


mat4 = int(1,3)
[

    [

        [
        1, 
            [
            2, 3
            ]
        ]
    ]
];

[1,2,3] 
LBRAC mat_rule RBRAC

*/