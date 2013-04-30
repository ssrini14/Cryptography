clc;
clear;
KeyHexArray = ['ff'; 'ff' ;'ff' ;'ff'; 'ff'; 'ff' ;'ff'; 'ff'; 'ff'; 'ff'; 'ff'; 'ff'; 'ff'; 'ff'; 'ff'; 'ff'];
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