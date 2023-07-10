
fileList = dir("*txt");

for i = 1:length(fileList)
    %%
    % Set up the Import Options and import the data
    opts = delimitedTextImportOptions("NumVariables", 5);

    % Specify read paramaters
    opts.DataLines = [18, Inf];
    opts.Delimiter = ",";
    opts.VariableNames = ["Time", "SlackExt", "Load", "Extension", "Separation"];
    opts.VariableTypes = ["double", "double", "double", "double", "double"];
    % Import the data
    %data = readtable("C:\Users\donatell\OneDrive - Chapman University\Research\SplittingHairs\02. Raw Data\Material Testing\Text Files\CRC1714_Ab.txt", opts);
    data = readtable(fileList(i).name, opts);

    % Convert to output type
    data = table2array(data);

    clear opts
    %%
    realData = data(:,[1,3,4]);

    %%
    numTests = ~isnan(realData(:,1));
    [row, col] = find(isnan(realData(:,1)));
    rowtest = row - [0; row(1:end-1)];
    I = find(rowtest ~= 1);

    testIndexEnd = row(I)-1;
    testIndexStart = [1; row(I(1:end-1))+16];

    individualTests = struct();
    %%

    fileNamePrefix = fileList(i).name(1:end-4);

    varNames = ["Time", "Load", "Extension"];
    %%

    for i = 1:length(testIndexEnd)
        individualTestData = realData(testIndexStart(i):testIndexEnd(i),:);
        individualTestData(any(isnan(individualTestData), 2), :) = [];
        individualTests.("test" + num2str(i)) = individualTestData;
        fileName = fileNamePrefix + "_" + "test" + num2str(i) + ".csv";
        T = array2table(individualTestData,'VariableNames', varNames);
        writetable(T,fileName)
    end

    save(fileNamePrefix+".mat", "individualTests");
    
end
