clc;
clear;
%01a3d965367112e6f482795093bc2633
KeyHexArray = ['01'; 'a3' ;'d9' ;'65'; '36'; '71' ;'12'; 'e6'; 'f4'; '82'; '79'; '50'; '93'; 'bc'; '26'; '33'];
fileName = reshape(KeyHexArray',1,[]).'.';
Key =hex2dec(KeyHexArray);
fid = fopen([fileName '.txt'], 'wt'); 
Hgt = Hight();
Hgt.setKey(Key);
Hgt.setRounds(1);
for incr1= 1:1:512
   PT = floor(rand(1,8)*255);
   [CT,CTHex,inputPT,PTHex] = Hgt.encrypt(PT);
   value = [PTHex ',' CTHex];
   fprintf(fid,value);
   fprintf(fid,'\n'); 
end    
fclose(fid);