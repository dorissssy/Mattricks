/* Addition */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = int(2,2)[[1,1],[1,1]];
  mat3 = mat1 + mat2;
  return mat3;
}

/* Subtraction */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = int(2,2)[[1,1],[1,1]];
  mat3 = mat1 - mat2;
  return mat3;
}

/* Multiplication */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = int(2,2)[[1,1],[1,1]];
  mat3 = mat1 * mat2;
  return mat3;
}

/* Division */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = int(2,2)[[1,1],[1,1]];
  mat3 = mat1 / mat2;
  return mat3;
}

/* Transpose */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = ^mat1;
  return mat2;
}

/* Eigenvector  */
function main() gives mat {
  mat1 = int(2,2)[[1,2],[3,4]];
  mat2 = ~mat1;
  return mat2;
}
