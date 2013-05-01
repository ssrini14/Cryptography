

classdef Hight < handle
    
    properties
        round = 32;
        BlockSize = 0;
        KeySize = 0;
        key = zeros(1,16);
        PT  = zeros(1,8);
        
    end
    
    methods (Static)
        
        function [BlockSize] = blockSize()
            BlockSize = 8;
            %val = Hgt.BlockSize;
        end
        
        function [KeySize] = keySize()
            KeySize = 16;
            %val = Hgt.KeySize;
        end
        
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
            WK = Hight.HextoBin(WKHex);
        end
        
        function [SK] = subKeyGeneration(Key)
            
            d{1,128} = {};
            SK{1,128} = {};
            S='1011010';
            d{1} = S;
            
            for incr1 = 1:1:127
                SNew = Hight.computeBitXOr(S(end-incr1-2),S(end-incr1+1));
                S = [SNew S];
                d{incr1+1} = ['0' S(end-incr1-6:end-incr1)];
            end
            
            for incr2 = 1:1:8
                for incr3 =1:1:8
                    index1 = 8 *(16 -mod((incr3-incr2),8));
                    index2 = 8 *(8-mod((incr3-incr2),8));
                    SK{(16*(incr2-1)) + incr3} = Hight.addMod8...
                        (Key(index1-7:index1),d{16*(incr2-1) + incr3});
                    SK{(16*(incr2-1)) +incr3+8} = Hight.addMod8...
                        (Key(index2-7:index2),d{16*(incr2-1) + incr3 +8});
                end
            end
        end
        
        %Method - shiftBits
        %Description - Rotate the bits to left or right
        
        %Parameter - BitVal
        %   Desc -Input Bit stream to be rotated
        %Parameter - shiftVal
        %   Desc - Integer indicating the rotation, positive is right shift and
        %   negative is left shift
        %Return - BitShiftVal
        %   Desc - Rotated Bits from the original bit stream
        
        function [BitShiftVal] = shiftBits(BitVal,shiftVal)
            
            Idx = 1:1:length(BitVal);
            SizeBit = length(BitVal);
            
            % shiftIdx = circshift(Idx',shiftVal)';
            % BitShiftVal = BitVal(shiftIdx);
            
            
            if(shiftVal < 0)
                shiftVal = -1 * shiftVal;
                shiftIdx  = Idx + shiftVal;
                shiftIdx(shiftIdx > SizeBit) = shiftIdx(shiftIdx > SizeBit) -SizeBit;
                BitShiftVal = BitVal(shiftIdx);
            else
                shiftIdx  = Idx - shiftVal;
                shiftIdx(shiftIdx <= 0) = shiftIdx(shiftIdx <= 0) + SizeBit;
                BitShiftVal = BitVal(shiftIdx);
            end
            
            
        end
        
        %Method - HextoBin
        %Description - Convert Hexadecimal String to Binary
        
        %Parameter - TextHex
        %   Desc -Hexadecimal String
        %Return - Textbin
        %   Desc - Binary string (equivalence) of TextHex
        
        function [Textbin] = HextoBin(TextHex)
            
            Textbin = [];
            
            for incr1 = 1:1:length(TextHex)
                %PTbin= [PTbin dec2bin(hex2dec(PTHex(incr1)),4)];
                switch TextHex(incr1)
                    case '0'
                        Textbin = [Textbin '0000'];
                    case '1'
                        Textbin = [Textbin '0001'];
                    case '2'
                        Textbin = [Textbin '0010'];
                    case '3'
                        Textbin = [Textbin '0011'];
                    case '4'
                        Textbin = [Textbin '0100'];
                    case '5'
                        Textbin = [Textbin '0101'];
                    case '6'
                        Textbin = [Textbin '0110'];
                    case '7'
                        Textbin = [Textbin '0111'];
                    case '8'
                        Textbin = [Textbin '1000'];
                    case '9'
                        Textbin = [Textbin '1001'];
                    case {'a','A'}
                        Textbin = [Textbin '1010'];
                    case {'b','B'}
                        Textbin = [Textbin '1011'];
                    case {'c','C'}
                        Textbin = [Textbin '1100'];
                    case {'d','D'}
                        Textbin = [Textbin '1101'];
                    case {'e','E'}
                        Textbin = [Textbin '1110'];
                    case {'f','F'}
                        Textbin = [Textbin '1111'];
                end
            end
        end
        
        
        %Method - BintoHex
        %Description - Convert Binary String to Hexadecimal
        
        %Parameter - PTBin
        %   Desc -Binary string
        %Return - PTHex
        %   Desc - Hexadecimal string (equivalence) of PTBin
        
        function [TextHex] = BintoHex(TextBin)
            
            TextHex = [];
            for incr1 = 1:4:length(TextBin)
                switch TextBin(incr1:incr1+3)
                    case '0000'
                        TextHex = [TextHex '0'];
                    case '0001'
                        TextHex = [TextHex '1'];
                    case '0010'
                        TextHex = [TextHex '2'];
                    case '0011'
                        TextHex = [TextHex '3'];
                    case '0100'
                        TextHex = [TextHex '4'];
                    case '0101'
                        TextHex = [TextHex '5'];
                    case '0110'
                        TextHex = [TextHex '6'];
                    case '0111'
                        TextHex = [TextHex '7'];
                    case '1000'
                        TextHex = [TextHex '8'];
                    case '1001'
                        TextHex = [TextHex '9'];
                    case '1010'
                        TextHex = [TextHex 'A'];
                    case '1011'
                        TextHex = [TextHex 'B'];
                    case '1100'
                        TextHex = [TextHex 'C'];
                    case '1101'
                        TextHex = [TextHex 'D'];
                    case '1110'
                        TextHex = [TextHex 'E'];
                    case '1111'
                        TextHex = [TextHex 'F'];
                end
            end
        end
        
        function [SumOut] = addMod8(X1,X2)
            SumOut = dec2bin(mod(bin2dec(X1) + bin2dec(X2),256),8);
        end
        
        function [SumOut] = subMod8(X1,X2)
            SumOut = dec2bin(mod(bin2dec(X1) - bin2dec(X2),256),8);
        end
        
        
        %Method - computeBitXOr
        %Description - Compute Bitwise XOR
        
        %Parameter - inpBits1
        %   Desc -Binary string from Input 1
        %Parameter - inpBits2
        %   Desc -Binary string from Input 2
        %Return - outputBit
        %   Desc - Bitwise Xored output Bits from Input 1 and Input 2
        
        function [outputBit] = computeBitXOr(inpBits1,inpBits2)
            lenBT1 = length(inpBits1);
            lenBT2 = length(inpBits2);
            
            if(lenBT1 > lenBT2)
                inpBits2 = [dec2bin(0,lenBT1-lenBT2) inpBits2];
            elseif(lenBT2 > lenBT1)
                inpBits1 = [dec2bin(0,lenBT2-lenBT1) inpBits1];
            end
            
            outputBit = sprintf('%d',abs(inpBits1-inpBits2));
            
            outputBit(strfind(outputBit,' ')) = [];
            
        end
        
        function [F0] = roundFunc0(X)
            
            X1 = Hight.shiftBits(X,-1);
            X2 = Hight.shiftBits(X,-2);
            X12 = Hight.computeBitXOr(X1,X2);
            X7 = Hight.shiftBits(X,-7);
            F0 = Hight.computeBitXOr(X12,X7);
        end
        
        function [F1] = roundFunc1(X)
            
            X3 = Hight.shiftBits(X,-3);
            X4 = Hight.shiftBits(X,-4);
            X34 = Hight.computeBitXOr(X3,X4);
            X6 = Hight.shiftBits(X,-6);
            F1 = Hight.computeBitXOr(X34,X6);
        end
        
        
    end
    
    methods
        
        function Hgt = Hight()
            Hgt.BlockSize = Hight.blockSize();
            Hgt.KeySize = Hight.keySize();
        end
        
        function setRounds(Hgt,inputRound)
            Hgt.round = inputRound;
        end
        
        function setKey(Hgt,inputKey)
            Hgt.key  = inputKey;
        end
        
        function [PTHex, KeyHex] = initialConversion(Hgt)
            
            if(length(Hgt.PT) ~= Hgt.BlockSize)
                error('Block Size does not match with specification');
            end
            
            if(length(Hgt.key) ~= Hgt.KeySize)
                error('Key Size does not match with specification');
            end
            
            if(Hgt.round < 0)
                error('# of rounds for the block cipher must be > 0');
            end
            
            PTHex = dec2hex(Hgt.PT,2);
            KeyHex = dec2hex(Hgt.key,2);
            PTHex = reshape(PTHex',1,[]).'.';
            KeyHex = reshape(KeyHex',1,[]).'.';
            PTHex(isspace(PTHex)) = [];
            KeyHex(isspace(KeyHex)) = [];
            
        end
        
        function [CT,CTHex,inputPT,PTHex] = encrypt(Hgt,inputPT)
            
            Hgt.PT = inputPT;
            [PTHex, KeyHex] = initialConversion(Hgt);
            WK = Hight.getWhiteningKey(KeyHex);
            KeyBin = Hight.HextoBin(KeyHex);
            SK = Hight.subKeyGeneration(KeyBin);
            PTBin =  Hight.HextoBin(PTHex);
            
            X = PTBin;
            
            X(1:8) = X(1:8);                          %7
            X(9:16) = Hight.computeBitXOr(X(9:16),WK(33:40)); %6
            X(17:24) = X(17:24);                      %5
            X(25:32) = Hight.addMod8(X(25:32),WK(41:48));    %4
            X(33:40) = X(33:40);                      %3
            X(41:48) = Hight.computeBitXOr(X(41:48),WK(49:56)); %2
            X(49:56) = X(49:56);                          %1
            X(57:64) = Hight.addMod8(X(57:64),WK(57:64));        %0
            
            Xnew = X;
            
            for incr1 = 1:1:Hgt.round
                Xnew(1:8) = X(9:16);
                Xnew(9:16) = Hight.addMod8(X(17:24),Hight.computeBitXOr...
                    (Hight.roundFunc1(X(25:32)),SK{4*(incr1-1)+3}));
                Xnew(17:24) = X(25:32);
                Xnew(25:32) = Hight.computeBitXOr(X(33:40),Hight.addMod8...
                    (Hight.roundFunc0(X(41:48)),SK{4*(incr1-1)+2}));
                Xnew(33:40) = X(41:48);
                Xnew(41:48) = Hight.addMod8(X(49:56),Hight.computeBitXOr...
                    (Hight.roundFunc1(X(57:64)),SK{4*(incr1-1)+1}));
                Xnew(49:56) = X(57:64);
                Xnew(57:64) = Hight.computeBitXOr(X(1:8),Hight.addMod8...
                    (Hight.roundFunc0(X(9:16)),SK{4*(incr1-1)+4}));
                X = Xnew;
            end
            
            %Swap at the end for final transformation
            X = [X(57:64) X(1:56)];
           
                
                %Final Transformation
                Xnew(1:8) = X(1:8);
                Xnew(9:16) = Hight.computeBitXOr(X(9:16),WK(1:8));
                Xnew(17:24) = X(17:24);
                Xnew(25:32) = Hight.addMod8(X(25:32),WK(9:16));
                Xnew(33:40) = X(33:40);
                Xnew(41:48) = Hight.computeBitXOr(X(41:48),WK(17:24));
                Xnew(49:56) = X(49:56);
                Xnew(57:64) = Hight.addMod8(X(57:64),WK(25:32));
                            
            
            CTHex = Hight.BintoHex(Xnew);
            CT = hex2dec(reshape(CTHex,2,[]).');
        end
        
        %Need work on it
        function [PT,PTHex,inputCT,CTHex] = decrypt(Hgt,inputCT)
            
            Hgt.PT = inputCT;
            [CTHex, KeyHex] = initialConversion(Hgt);
            WK = Hight.getWhiteningKey(KeyHex);
            KeyBin = Hight.HextoBin(KeyHex);
            SK = Hight.subKeyGeneration(KeyBin);
            CTBin =  Hight.HextoBin(CTHex);
            
            X = CTBin;
            %Final Transofrmation of encryption
            Xnew(1:8) =X(1:8);
            Xnew(9:16) = Hight.computeBitXOr(X(9:16),WK(1:8));
            Xnew(17:24) = X(17:24);
            Xnew(25:32) = Hight.subMod8(X(25:32),WK(9:16));
            Xnew(33:40) = X(33:40);
            Xnew(41:48) = Hight.computeBitXOr(X(41:48),WK(17:24));
            Xnew(49:56) = X(49:56);
            Xnew(57:64) = Hight.subMod8(X(57:64),WK(25:32));
            
            X = Xnew;
            
            %Last Iteration of encrption
            
            Xnew(1:8) = Hight.computeBitXOr(X(1:8),Hight.addMod8...
                (Hight.roundFunc0(X(9:16)),SK{128}));
            Xnew(9:16) = X(9:16);
            Xnew(17:24) = Hight.subMod8(X(17:24),Hight.computeBitXOr...
                (Hight.roundFunc1(X(25:32)),SK{127}));
            Xnew(25:32) = X(25:32);
            Xnew(33:40) = Hight.computeBitXOr(X(33:40),Hight.addMod8...
                (Hight.roundFunc0(X(41:48)),SK{126}));
            Xnew(41:48) = X(41:48);
            Xnew(49:56) = Hight.subMod8(X(49:56),Hight.computeBitXOr...
                (Hight.roundFunc1(X(57:64)),SK{125}));
            Xnew(57:64) = X(57:64);
            
            X = Xnew;
            
            for incr1 = Hgt.round-1:-1:1
                
                Xnew(9:16) = X(1:8);
                Xnew(17:24) = Hight.subMod8(X(9:16),Hight.computeBitXOr...
                    (Hight.roundFunc1(X(17:24)),SK{4*(incr1-1)+3}));
                Xnew(25:32) = X(17:24);
                Xnew(33:40) = Hight.computeBitXOr(X(25:32),Hight.addMod8...
                    (Hight.roundFunc0(X(33:40)),SK{4*(incr1-1)+2}));
                Xnew(41:48) = X(33:40);
                Xnew(49:56) = Hight.subMod8(X(41:48),Hight.computeBitXOr...
                    (Hight.roundFunc1(X(49:56)),SK{4*(incr1-1)+1}));
                Xnew(57:64) = X(49:56);
                Xnew(1:8) = Hight.computeBitXOr(X(57:64),Hight.addMod8...
                    (Hight.roundFunc0(X(1:8)),SK{4*(incr1-1)+4}));
                
                X = Xnew;
                
            end
            
            %First iteration of encryption
            
            X(1:8) = X(1:8);                          %7
            X(9:16) = Hight.computeBitXOr(X(9:16),WK(33:40)); %6
            X(17:24) = X(17:24);                      %5
            X(25:32) = Hight.subMod8(X(25:32),WK(41:48));    %4
            X(33:40) = X(33:40);                      %3
            X(41:48) = Hight.computeBitXOr(X(41:48),WK(49:56)); %2
            X(49:56) = X(49:56);                          %1
            X(57:64) = Hight.subMod8(X(57:64),WK(57:64));        %0
            
            Xnew = X;
            
            PTHex = Hight.BintoHex(Xnew);
            PT = hex2dec(reshape(PTHex,2,[]).');
        end
        
        
    end
end