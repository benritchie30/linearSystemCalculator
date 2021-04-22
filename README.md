# linear System Solution Calculator
This program takes an input of a system of linear equations through a text file. 
Then it performs a recursive gaussian elimination algorithm until it is in reduced echelon form.
It can also check for the cases where there are no solutions or infinite solutions.

Example Output:
```
Enter input file name
matrix2.txt

Input Matrix:
[15.0,-12.0,11.0,8.0,30.0]
[7.0,18.0,-5.0,13.0,24.0]
[5.0,5.0,3.0,5.0,9.0]
[19.0,18.0,5.0,8.0,-2.0]

Recuced Echelon Form:
[1.0,0.0,0.0,0.0,-0.5492957746478879]
[0.0,1.0,0.0,0.0,-1.031924882629108]
[0.0,0.0,1.0,0.0,-0.19248826291079868]
[-0.0,-0.0,-0.0,1.0,3.496713615023475]

Solutions:
x0 = -0.5492957746478879
x1 = -1.031924882629108
x2 = -0.19248826291079868
x3 = 3.496713615023475
```
Example Output with infinite solutions:
```
Enter input file name
matrixInf.txt

Input Matrix:
[1.0,2.0,3.0,4.0]
[2.0,4.0,6.0,8.0]
[6.0,4.0,2.0,5.0]

Matrix has infinitely many solutions

Gausian Elimination Paused at:
[1.0,2.0,3.0,4.0]
[0.0,0.0,0.0,0.0]
[0.0,-8.0,-16.0,-19.0]
```
