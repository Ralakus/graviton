#!/usr/bin/env sh

FAILED=0

cargo run test/1.grav
if [ $? -eq 1 ]
then
  echo "Failed test 1"
  FAILED=1
fi

cargo run test/2.grav
if [ $? -eq 1 ]
then
  echo "Failed test 2"
  FAILED=1
fi

cargo run test/3.grav
if [ $? -eq 1 ]
then
  echo "Failed test 3"
  FAILED=1
fi

cargo run test/4.grav -e=ast --no_run
if [ $? -eq 1 ]
then
  echo "Failed test 4"
  FAILED=1
fi

cargo run test/5.grav -e=ast --no_run
if [ $? -eq 1 ]
then
  echo "Failed test 5"
  FAILED=1
fi

cargo run test/6.grav -e=ast --no_run
if [ $? -eq 1 ]
then
  echo "Failed test 6"
  FAILED=1
fi

cargo run test/7.grav -e=ast --no_run
if [ $? -eq 1 ]
then
  echo "Failed test 7"
  FAILED=1
fi

echo "14" | cargo run examples/fib.grav
if [ $? -eq 1 ]
then
  echo "Failed fibonacci example"
  FAILED=1
fi

if [ $FAILED -eq 1 ]
then
    exit 1
fi