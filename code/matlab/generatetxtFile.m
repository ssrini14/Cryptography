%Script for generating Plain text and Cipher text pairs

clc;
clear;

%Enter the Master Key in hexadecimal
KeyHex = '00112233445566778899aabbccddeeff';

%convert from Hexadecimal to byte array
Key = hex2dec(reshape(KeyHex,2,[]).');
%Create a file with the same name as the Master Key
fid = fopen([KeyHex '.txt'], 'wt'); 

%instantiate the Hight class and set the key and number of rounds
Hgt = Hight();
Hgt.setKey(Key);
Hgt.setRounds(1);

%create 512 Plain Text and Cipher Text pairs
for incr1= 1:1:512
    
   %Generate a random Plain Text 
   PT = floor(rand(1,8)*255);
   %Perform Hight encryption
   [CT,CTHex,inputPT,PTHex] = Hgt.encrypt(PT);
   %Have a comma seperated Plain text and Cipher text value
   value = [PTHex ',' CTHex];
   %Write it to the file
   fprintf(fid,value);
   fprintf(fid,'\n'); 
end    
%Close the file
fclose(fid);