#!/usr/bin/env sh

FAILED=0

cargo run run test/1.grav
if [ $? -eq 1 ]
then
  echo "Failed test 1"
  FAILED=1
fi

cargo run run test/2.grav
if [ $? -eq 1 ]
then
  echo "Failed test 2"
  FAILED=1
fi

cargo run run test/3.grav
if [ $? -eq 1 ]
then
  echo "Failed test 3"
  FAILED=1
fi

cargo run test/4.grav -e=ast -o=out.gast
if [ $? -eq 1 ]
then
  echo "Failed test 4"
  FAILED=1
fi

cargo run test/5.grav -e=ast -o=out.gast
if [ $? -eq 1 ]
then
  echo "Failed test 5"
  FAILED=1
fi

cargo run test/6.grav -e=ast -o=out.gast
if [ $? -eq 1 ]
then
  echo "Failed test 6"
  FAILED=1
fi

cargo run test/7.grav -e=ast -o=out.gast
if [ $? -eq 1 ]
then
  echo "Failed test 7"
  FAILED=1
fi

cargo run run test/8.grav
if [ $? -eq 1 ]
then
  echo "Failed test 8"
  FAILED=1
fi

cargo run run test/9.grav
if [ $? -eq 1 ]
then
  echo "Failed test 9"
  FAILED=1
fi

echo "14" | cargo run run test/10.grav
if [ $? -eq 1 ]
then
  echo "Failed test 10"
  FAILED=1
fi

cargo run run test/11.grav
if [ $? -eq 1 ]
then
  echo "Failed test 11"
  FAILED=1
fi

cargo run run test/12.grav
if [ $? -eq 1 ]
then
  echo "Failed test 12"
  FAILED=1
fi

cargo run run test/13.grav
if [ $? -eq 1 ]
then
  echo "Failed test 13"
  FAILED=1
fi

cargo run run test/14.grav
if [ $? -eq 1 ]
then
  echo "Failed test 14"
  FAILED=1
fi

cargo run run test/15.grav
if [ $? -eq 1 ]
then
  echo "Failed test 15"
  FAILED=1
fi

echo "14" | cargo run run examples/fib.grav
if [ $? -eq 1 ]
then
  echo "Failed fibonacci example"
  FAILED=1
fi

rm out.gast

if [ $FAILED -eq 1 ]
then
  exit 1
else
  echo
  echo "All tests passed"
fi