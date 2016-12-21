v = zeros(3, 1);
A = [1 1 1; 2 2 2; 3 3 3]
for i = 1:3
  for j = 1:3
    v(i) = v(i) + A(i, j) * x(j);
  end
end