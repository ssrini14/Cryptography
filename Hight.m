%%Forezen HIGHT

function [ CTHex ] = Hight(PTHex, KeyHex, round,operation)
%UNTITLED18 Summary of this function goes here
%   Detailed explanation goes here

PTHex(isspace(PTHex)) = [];
KeyHex(isspace(KeyHex)) = [];

PT = HextoBin(PTHex);
Key = HextoBin(KeyHex);

WK = getWhiteningKey(KeyHex);
SK = subKeyGeneration(Key);

if(strcmpi(operation,'encrypt'))
  CT = encrypt(PT,round,SK,WK);
else
  CT = decrypt(PT,round,SK,WK);   
end

CTHex = BintoHex(CT);

function [CT] = encrypt(PT,round,SK,WK)
X = PT;

X(1:8) = X(1:8);                          %7
X(9:16) = computeBitXOr(X(9:16),WK(33:40)); %6
X(17:24) = X(17:24);                      %5     
X(25:32) = addMod8(X(25:32),WK(41:48));    %4 
X(33:40) = X(33:40);                      %3 
X(41:48) = computeBitXOr(X(41:48),WK(49:56)); %2
X(49:56) = X(49:56);                          %1
X(57:64) = addMod8(X(57:64),WK(57:64));        %0     

Xnew = X;

for incr1 = 1:1:round-1
    Xnew(1:8) = X(9:16);
    Xnew(9:16) = addMod8(X(17:24),computeBitXOr(roundFunc1(X(25:32)),SK{4*(incr1-1)+3}));
    Xnew(17:24) = X(25:32);
    Xnew(25:32) = computeBitXOr(X(33:40),addMod8(roundFunc0(X(41:48)),SK{4*(incr1-1)+2}));
    Xnew(33:40) = X(41:48);
    Xnew(41:48) = addMod8(X(49:56),computeBitXOr(roundFunc1(X(57:64)),SK{4*(incr1-1)+1}));
    Xnew(49:56) = X(57:64);
    Xnew(57:64) = computeBitXOr(X(1:8),addMod8(roundFunc0(X(9:16)),SK{4*(incr1-1)+4}));
    X = Xnew;    
end 

%Last iteration
 Xnew(1:8) = computeBitXOr(X(1:8),addMod8(roundFunc0(X(9:16)),SK{128}));
 Xnew(9:16) = X(9:16);
 Xnew(17:24) = addMod8(X(17:24),computeBitXOr(roundFunc1(X(25:32)),SK{127}));
 Xnew(25:32) = X(25:32);
 Xnew(33:40) = computeBitXOr(X(33:40),addMod8(roundFunc0(X(41:48)),SK{126}));
 Xnew(41:48) = X(41:48);
 Xnew(49:56) = addMod8(X(49:56),computeBitXOr(roundFunc1(X(57:64)),SK{125}));
 Xnew(57:64) = X(57:64);
 X = Xnew;
 
 
 %Final Transformation
 Xnew(1:8) =X(1:8);
 Xnew(9:16) = computeBitXOr(X(9:16),WK(1:8));
 Xnew(17:24) = X(17:24);
 Xnew(25:32) = addMod8(X(25:32),WK(9:16));
 Xnew(33:40) = X(33:40);
 Xnew(41:48) = computeBitXOr(X(41:48),WK(17:24));
 Xnew(49:56) = X(49:56);
 Xnew(57:64) = addMod8(X(57:64),WK(25:32));

 CT = Xnew;

%Need work on it 
function [PT] = decrypt(CT,round,SK,WK)

X = CT;
%Final Transofrmation of encryption
 Xnew(1:8) =X(1:8);
 Xnew(9:16) = computeBitXOr(X(9:16),WK(1:8));
 Xnew(17:24) = X(17:24);
 Xnew(25:32) = subMod8(X(25:32),WK(9:16));
 Xnew(33:40) = X(33:40);
 Xnew(41:48) = computeBitXOr(X(41:48),WK(17:24));
 Xnew(49:56) = X(49:56);
 Xnew(57:64) = subMod8(X(57:64),WK(25:32));

X = Xnew;

%Last Iteration of encrption

 Xnew(1:8) = computeBitXOr(X(1:8),addMod8(roundFunc0(X(9:16)),SK{128}));
 Xnew(9:16) = X(9:16);
 Xnew(17:24) = subMod8(X(17:24),computeBitXOr(roundFunc1(X(25:32)),SK{127}));
 Xnew(25:32) = X(25:32);
 Xnew(33:40) = computeBitXOr(X(33:40),addMod8(roundFunc0(X(41:48)),SK{126}));
 Xnew(41:48) = X(41:48);
 Xnew(49:56) = subMod8(X(49:56),computeBitXOr(roundFunc1(X(57:64)),SK{125}));
 Xnew(57:64) = X(57:64);

X = Xnew;

for incr1 = round-1:-1:1

    Xnew(9:16) = X(1:8);
    Xnew(17:24) = subMod8(X(9:16),computeBitXOr(roundFunc1(X(17:24)),SK{4*(incr1-1)+3}));
    Xnew(25:32) = X(17:24);
    Xnew(33:40) = computeBitXOr(X(25:32),addMod8(roundFunc0(X(33:40)),SK{4*(incr1-1)+2}));
    Xnew(41:48) = X(33:40);
    Xnew(49:56) = subMod8(X(41:48),computeBitXOr(roundFunc1(X(49:56)),SK{4*(incr1-1)+1}));
    Xnew(57:64) = X(49:56);
    Xnew(1:8) = computeBitXOr(X(57:64),addMod8(roundFunc0(X(1:8)),SK{4*(incr1-1)+4}));
    
    X = Xnew;  
    
end  
 
%First iteration of encryption

X(1:8) = X(1:8);                          %7
X(9:16) = computeBitXOr(X(9:16),WK(33:40)); %6
X(17:24) = X(17:24);                      %5     
X(25:32) = subMod8(X(25:32),WK(41:48));    %4 
X(33:40) = X(33:40);                      %3 
X(41:48) = computeBitXOr(X(41:48),WK(49:56)); %2
X(49:56) = X(49:56);                          %1
X(57:64) = subMod8(X(57:64),WK(57:64));        %0     

Xnew = X;

PT = Xnew;

function [F0] = roundFunc0(X)

X1 = shiftBits(X,-1);
X2 = shiftBits(X,-2);
X12 = computeBitXOr(X1,X2);
X7 = shiftBits(X,-7);
F0 = computeBitXOr(X12,X7);


function [F1] = roundFunc1(X)

X3 = shiftBits(X,-3);
X4 = shiftBits(X,-4);
X34 = computeBitXOr(X3,X4);
X6 = shiftBits(X,-6);
F1 = computeBitXOr(X34,X6);

function [WK] = getWhiteningKey(KeyHex)

WKHex = KeyHex(1:16);

for incr1=1:1:4
  indexS = 2 *(incr1+12);
  indexD = 2 *incr1;
  WKHex(indexD-1:indexD) = KeyHex(indexS-1:indexS);
end    
for incr1=5:1:8
  indexS = 2 *(incr1-4);
  indexD = 2 *incr1;  
  WKHex(indexD-1:indexD) = KeyHex(indexS-1:indexS);
end
WK = HextoBin(WKHex);



function [SK] = subKeyGeneration(Key)

d{1,128} = {};
SK{1,128} = {};
S='1011010';
d{1} = S;

for incr1 = 1:1:127
  SNew = computeBitXOr(S(end-incr1-2),S(end-incr1+1));
  S = [SNew S]; 
  d{incr1+1} = ['0' S(end-incr1-6:end-incr1)];
end    
try
for incr2 = 1:1:8
   for incr3 =1:1:8 
      index1 = 8 *(16 -mod((incr3-incr2),8));
      index2 = 8 *(8-mod((incr3-incr2),8));
      SK{(16*(incr2-1)) + incr3} = addMod8(Key(index1-7:index1),d{16*(incr2-1) + incr3});
      SK{(16*(incr2-1)) +incr3+8} = addMod8(Key(index2-7:index2),d{16*(incr2-1) + incr3 +8});
   end
end
catch ex
   disp(incr2);
   disp(incr3);
end    

