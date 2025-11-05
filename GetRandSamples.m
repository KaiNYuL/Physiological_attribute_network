% Get a random sample R and find nearest neighbors
% D: training set
% D1: dataset of class 1
% D2: dataset of class 2
% Dh: distances of features between R and its nearest neighbors from the same class
% Dm: distances of features between R and its nearest neighbors from a different class
function [R, Dh, Dm] = GetRandSamples(D, D1, D2, k)
    % Generate a random number to select sample R
    r = ceil(1 + (size(D, 1) - 1) * rand);
    R = D(r, :); % Assign the r-th row to R

    % Initialize distance arrays
    d1 = zeros(1, 0); % Distances from R to samples in D1
    d2 = zeros(1, 0); % Distances from R to samples in D2

    % Calculate distances from R to all samples in D1
    for i = 1 : size(D1, 1)
        d1(1, i) = norm(R - D1(i, :));
    end

    % Calculate distances from R to all samples in D2
    for j = 1 : size(D2, 1)
        d2(1, j) = norm(R - D2(j, :));
    end

    % Sort distances
    [v1, L1] = sort(d1); % Sort distances to D1 samples
    [v2, L2] = sort(d2); % Sort distances to D2 samples

    % Determine nearest neighbors based on R's class
    % Class determination criterion may vary depending on classification method
    if R(1, size(R, 2)) < 0  % If R belongs to class 1
        H = D1(L1(1, 2 : k + 1), :); % k nearest neighbors from the same class (excluding R itself)
        M = D2(L2(1, 1 : k), :);     % k nearest neighbors from the other class
    else                      % If R belongs to class 2
        H = D1(L1(1, 1 : k), :);     % k nearest neighbors from the other class
        M = D2(L2(1, 2 : k + 1), :); % k nearest neighbors from the same class (excluding R itself)
    end

    % Calculate feature-wise distances: (feature1 - feature2) / (max - min)
    % For this dataset, values range from 1 to 10, so max - min = 9
    for i = 1 : size(H, 1)
        for j = 1 : size(H, 2)
            Dh(i, j) = abs(H(i, j) - R(1, j)) / 9;
            Dm(i, j) = abs(M(i, j) - R(1, j)) / 9;
        end
    end
end