parfor i = 1:100
    c(i) = max(eig(rand(1000)));
end