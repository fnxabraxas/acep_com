

function plot_legend


h=figure(1);
set(h(1),'WindowStyle','normal');
set(h(1),'Position',[1 1 700 700]);
set(h(1),'PaperPosition',[0 0 15 15]);
clf reset;


subplot(1,1,1); axis off;
h=rectangle('Position',[0 0 1 1]);
box
%set(h(1),'FaceColor',[1 1 1]);
hold all;

for k=1:4
    [colorS,binS]=Scolor(k);
    h=rectangle('Position',[0.03+(k-1)*0.25 0.7  0.1 0.2]);
    set(h(1),'FaceColor',colorS);
    text(0.15+(k-1)*0.25, 0.79,['\fontsize{13}' binS]);
end
for k=5:8
    [colorS,binS]=Scolor(k);
    h=rectangle('Position',[0.03+(k-5)*0.25 0.4  0.1 0.2]);
    set(h(1),'FaceColor',colorS);
    text(0.15+(k-5)*0.25, 0.49,['\fontsize{13}' binS]);
end
for k=9:10
    [colorS,binS]=Scolor(k);
    h=rectangle('Position',[0.03+(k-9)*0.25 0.1  0.1 0.2]);
    set(h(1),'FaceColor',colorS);
    text(0.15+(k-9)*0.25, 0.19,['\fontsize{13}' binS]);
end

    h=patch([0.03+(11-9)*0.25 0.03+(11-9)*0.25+0.1 0.03+(11-9)*0.25+0.1 0.03+(11-9)*0.25], [0.1 0.1 0.1+0.2 0.1+0.2 ], [0 0 0]);
    hatchfill(h,'speckle',50,0.3)
    text(0.15+(11-9)*0.25, 0.19,['\fontsize{13}' '\bf{Ap}']);
    h=patch([0.03+(12-9)*0.25 0.03+(12-9)*0.25+0.1 0.03+(12-9)*0.25+0.1 0.03+(12-9)*0.25], [0.1 0.1 0.1+0.2 0.1+0.2 ], [0 0 0]);
    text(0.15+(12-9)*0.25, 0.19,['\fontsize{13}' '\bf{N-Ap}']);
    rectangle('Position',[0.52 0.06 0.47 0.28]);


clear;

end


function [colorS,binS]=Scolor(k)

switch k
    case 1
        colorS=[0.2 0.4 0];
        binS='\bf{P C C}';
    case 2
        colorS=[0 1 0];
        binS='\bf{P C D}';
    case 3
        colorS=[1 0.9 0.8];
        binS='\bf{P D C}';
    case 4
        colorS=[0.5 0 0.9];
        binS='\bf{P D D}';
    case 5
        colorS=[1 1 0];
        binS='\bf{A C C}';
    case 6
        colorS=[1 0.7 0.3];
        binS='\bf{A C D}';
    case 7
        colorS=[0.9 0.9 0.9];
        binS='\bf{A D C}';
    case 8
        colorS=[0.8 0.6 0.7];
        binS='\bf{A D D}';
    case 9
        colorS=[0 0.5 0.9];
        binS='\bf{N C}';
    case 10
        colorS=[0 0 1];
        binS='\bf{N D}';
        
        
%    case 11
%        colorS=[1 0.8 1];
%        binS='\bf{1010}';
%    case 12
%        colorS=[0.6 0.5 0.4];
%        binS='\bf{1011}';
%    case 13
%        colorS=[0 0.8 1];
%        binS='\bf{1100}';
%    case 14
%        colorS=[0.6 0.2 0];
%        binS='\bf{1101}';
%    case 15
%        colorS=[0.78 0.12 0.04 ];
%        binS='\bf{1110}';
%    case 16
%        colorS=[1 0 0];
%        binS='\bf{1111}';       
end 

end

