/* The GCD algorithm in MicroC */




function fname (int x) gives int {

    mat1 = int(0,0)[];
    mat2a = int(1,1)[[1],];
 
    mat2b = int(2,1)[[1], [1]];
    

    return x;
}


/* TODO:


   mat3 = int(1,3)[ [1,2,3] ];


    mat4 = int(1, 3)[ [1,2,3] ];

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