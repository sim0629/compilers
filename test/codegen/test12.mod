//
// test12
//
// function return values
//
// expected output: returned values are stored according to 
// their size
//

module test12;

function Int():integer;
begin
end Int;

function Char(): char;
begin
end Char;

function Bool(): boolean;
begin
end Bool;

procedure Test();
var b: boolean;
    c: char;
    i: integer;
begin
  b := Bool();
  c := Char();
  i := Int()
end Test;

begin
  Test()
end test12.
