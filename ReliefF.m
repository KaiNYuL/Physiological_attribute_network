% Implementation of ReliefF algorithm
% D: input training set (without identity information)
% k: number of nearest neighbors
function W = ReliefF(D, m, k)
    Rows = size(D, 1); % number of samples
    Cols = size(D, 2); % number of features (including class label column)

    % Proportion of class 2 and class 4 in the dataset
    type2 = sum((D(:, Cols) == 2)) / Rows;
    type4 = sum((D(:, Cols) == 4)) / Rows;

    % Split dataset into two classes for faster computation
    D1 = zeros(0, Cols); % class 1
    D2 = zeros(0, Cols); % class 2

    for i = 1 : Rows
        % Here you need a criterion to classify the samples
        % The rule can vary depending on the classification method
        % After normalization, class 1 < 0, class 2 >= 0
        if D(i, Cols) < 0
            D1(size(D1, 1) + 1, :) = D(i, :);
        elseif D(i, Cols) >= 0
            D2(size(D2, 1) + 1, :) = D(i, :);
        end
    end

    W = zeros(1, Cols); % Initialize feature weights to 0

    for i = 1 : m % Perform m iterations of sample selection
        % Randomly select a sample R from D
        [R, Dh, Dm] = GetRandSamples(D, D1, D2, k);

        % Update feature weights
        for j = 1 : length(W) % Accumulate for each feature
            W(1, j) = W(1, j) - sum(Dh(:, j)) / (k * m) + sum(Dm(:, j)) / (k * m);
        end
    end
end