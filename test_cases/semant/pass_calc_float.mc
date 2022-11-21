/* Addition */
function main() gives float {
  a = float 123.45;
  b = float 678.90;
  c = a + b;
  return c;
}

function main_2() gives float {
  a = float -123.;
  b = float -456.;
  c = a + b;
  return c;
}

/* Subtraction */
function main2() gives float {
  a = float 123.;
  b = float 456.;
  return b - a;
}

function main2_2() gives float {
  a = float 123.;
  b = float -456.;
  return b - a;
}

function main2_3() gives float {
  a = float -123.;
  b = float -456.;
  return b - a;
}

/* Multiplication */
function main3() gives float {
  a = float 123.;
  b = float 456.;
  c = a * b;
  return c;
}

function main3_2() gives float {
  a = float -123.;
  b = float 456.;
  c = a * b;
  return c;
}

function main3_3() gives float {
  a = float -123.;
  b = float -456.;
  c = a * b;
  return c;
}

/* Division */
function main4() gives float {
  a = float 123.;
  b = float 456.;
  c = b / a;
  return c;
}

function main4_1() gives float {
  a = float 123.;
  b = float 456.;
  c = a / b;
  return c;
}

function main4_2() gives float {
  a = float 123.;
  b = float -456.;
  c = b / a;
  return c;
}

function main4_3() gives float {
  a = float -123.;
  b = float -456.;
  c = b / a;
  return c;
}

/* Modulus */
function main5() gives float {
  a = float 2.;
  b = float 5.;
  c = b % 2;
  return c;
}
