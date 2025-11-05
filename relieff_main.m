% Main function
clear; clc;

filename = ("\hahv_halv_reliefF.csv");
data = readcsv(filename);

% Replace NaN values with 0
data(isnan(data)) = 0;

% Perform z-score normalization
data = zScoreNormalization(data);

% Use all columns as features (including class label column)
D = data;

m = 80;   % Number of sampling iterations
k = 8;    % Number of nearest neighbors
N = 20;   % Number of runs

% Run ReliefF algorithm N times
for i = 1 : N
    W(i, :) = ReliefF(D, m, k);
end

% Plot feature weights for each run to observe overall performance
for i = 1 : N
    plot(1 : size(W, 2), W(i, :));
    hold on;
end
xlabel('Feature index');
ylabel('Feature weight');
title('Global feature weights computed by ReliefF algorithm');

% Calculate the average weight for each feature across N runs
for i = 1 : size(W, 2)
    result(1, i) = sum(W(:, i)) / size(W, 1);
end
result = result';

% Plot the average feature weights
figure;
plot(result);
title('Average global feature weights computed by ReliefF algorithm');
grid on;
axis([1 48 -0.015 0.010]);
xlabel('Feature index');
ylabel('Average feature weight');