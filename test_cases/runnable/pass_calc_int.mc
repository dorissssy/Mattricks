int c;

function main() gives int {
  console << main1();
  console << main1_2();
  console << main2();
  console << main2_2();
  console << main2_3();
  console << main3();
  console << main3_2();
  console << main3_3();
  console << main4();
  console << main4_2();
  console << main4_3();
  /*console << main5();*/
  return 0;
}

/* Addition */
function main1() gives int {
  a = int 123;
  b = int 456;
  c = a + b;
  return c;
}

function main1_2() gives int {
  a = int -123;
  b = int -456;
  c = a + b;
  return c;
}

/* Subtraction */
function main2() gives int {
  a = int 123;
  b = int 456;
  return b - a;
}

function main2_2() gives int {
  a = int 123;
  b = int -456;
  return b - a;
}

function main2_3() gives int {
  a = int -123;
  b = int -456;
  return b - a;
}

/* Multiplication */
function main3() gives int {
  a = int 123;
  b = int 456;
  c = a * b;
  return c;
}

function main3_2() gives int {
  a = int -123;
  b = int 456;
  c = a * b;
  return c;
}

function main3_3() gives int {
  a = int -123;
  b = int -456;
  c = a * b;
  return c;
}

/* Division */
function main4() gives int {
  a = int 123;
  b = int 456;
  c = b / a;
  return c;
}

function main4_1() gives int {
  a = int 123;
  b = int 456;
  c = a / b;
  return c;
}

function main4_2() gives int {
  a = int 123;
  b = int -456;
  c = b / a;
  return c;
}

function main4_3() gives int {
  a = int -123;
  b = int -456;
  c = b / a;
  return c;
}

/* Modulus */
/*function main5() gives int {
  a = int 2;
  b = int 5;
  c = b % 2;
  return c;
}*/
