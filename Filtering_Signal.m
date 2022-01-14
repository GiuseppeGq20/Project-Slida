%%              FILTERING SIGNALS
clc; clear; close all;
% initialization useful variables
FR=60*10^3;
sens=[20:22,24,25,42:49];
PP={'PRE';'POST'};  pp={'pre';'post'};  formatspec={'.-r';'b'};
% loop for each sensor
for isens=1:length(sens)
    % identify Sensor and define a figure
    NameSensore=['A',num2str(sens(isens))];
    f=figure('Name',NameSensore);
    f.Units='normalized';
    f.Position=[0.1 0.1 0.6 0.8];
    % loop Pre and Post
    for jpp=1:2
        % loop for each 10 signals in the folder
        for i=1:10
            subplot(5,2,i);
            nomefilesignal=[NameSensore,' ',PP{jpp},'/',...
                NameSensore,'_',pp{jpp},'_',num2str(i),'.csv'];
            signal = csvread(nomefilesignal);       % import signals
            % filtering
            f1 = signal(:,end-3);  t=signal(:,1);
            % shift
            f1=f1-mean(f1(t<0));
            % riphase
            
            % filtering
            [~,idxmax]=max(f1);
            f1(abs(t-t(idxmax))>2.25/FR)=0;            

            hold on;
            plot(t,f1,formatspec{jpp},'DisplayName',PP{jpp});
            legend
        end
    end
end

