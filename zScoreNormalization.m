function normalizedData = zScoreNormalization(data)
% zScoreNormalization performs z-score normalization on each column of the input matrix data.
% Input:
%   data - an n×m matrix, where n is the number of observations and m is the number of features.
% Output:
%   normalizedData - the normalized data.

% Check if data is empty
if isempty(data)
    error('Input data cannot be empty.');
end

% Calculate the mean of each column
meanData = mean(data);

% Calculate the standard deviation of each column
stdData = std(data);

% Avoid division by zero
stdData(stdData == 0) = 1;

% Compute z-score
normalizedData = (data - meanData) ./ stdData;
end