%%Author - Srinivas Sridharan
%Date - 04-25-2013
%Class - Hight
%Description - This class implements the Hight algorithm and it has methods
%              to set the number of rounds, Key and perform encryption and
%              decryption
%Source -  http://www.iacr.org/cryptodb/archive/2006/CHES/04/04.pdf
%          http://tools.ietf.org/html/draft-kisa-hight-00     

classdef Hight < handle
    
    %List of class level properties
    properties
        round = 32;
        BlockSize = 0;
        KeySize = 0;
        key = zeros(1,16);
        PT  = zeros(1,8);
    end
    
    %List of static functions
    methods (Static)
        
        %Method - blockSize
        %Description - This method provides the block size for the
        %              cipher in number of bytes
                
        %Return - BlockSize
        %   Desc - Block size in bytes for the cipher
        
        function [BlockSize] = blockSize()
            BlockSize = 8;
        end
        
        %Method - keySize
        %Description - This method provides the key size for the
        %              cipher in number of bytes
                
        %Return - KeySize
        %   Desc - Key size in bytes for the cipher
        
        function [KeySize] = keySize()
            KeySize = 16;
        end
        
        %Method - getWhiteningKey
        %Description - This method gives the whitening Keys WK0-7 from the
        %              original Master key KeyHex
        
        %Parameter - KeyHex
        %   Desc -Original master key in Hexadecimal        
        %Return - WK
        %   Desc - Whitening Key in binary : WK(57:64) = WK0...WK(1:8) = WK7
        
        function [WK] = getWhiteningKey(KeyHex)
            
            WKHex = KeyHex(1:16);
            
            %MasterKey MK0-3 = WK4-7 and MK12-15=WK0-3
            %Extract the Whitening key in Hexadecimal
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
            %Convert the Whitening key from Hexadecimal to Binary
            WK = Hight.HextoBin(WKHex);
        end
        
        %Method - subKeyGeneration
        %Description - This method gives the sub Keys WK0-127 from the
        %              original Master key Key in binary
        
        %Parameter - Key
        %   Desc -Original master key in binary
        %Return - SK
        %   Desc - Subkey 0-128 in binary extracted from the MasterKey Key
        
        function [SK] = subKeyGeneration(Key)
            
            %initialize the empty array d and SK
            d{1,128} = {};
            SK{1,128} = {};
            %Seed to the 7 bit LFSR
            S='1011010';
            d{1} = S;
            
            %Generate all the 128 values of LFSR
            for incr1 = 1:1:127
                SNew = Hight.computeBitXOr(S(end-incr1-2),S(end-incr1+1));
                S = [SNew S];
                d{incr1+1} = ['0' S(end-incr1-6:end-incr1)];
            end
            
            %Generate the Subkeys from the output of LFSR
            %Refer to http://www.iacr.org/cryptodb/archive/2006/CHES/04/04.pdf
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
            
            %get the all indicies of BitVal in ascending
            Idx = 1:1:length(BitVal);
            SizeBit = length(BitVal);
            
            %if left shift
            if(shiftVal < 0)
                shiftVal = -1 * shiftVal;
                shiftIdx  = Idx + shiftVal;
                %simple bit index rotation
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
        
        %Method - addMod8
        %Description - Perform addition modulus 8
        
        %Parameter - X1
        %   Desc -Binary string as Input 1
        %Parameter - X2
        %   Desc -Binary string as Input 2
        %Return - SumOut
        %   Desc - Add X1 and X2 and then perform modulus 8
        
        function [SumOut] = addMod8(X1,X2)
            SumOut = dec2bin(mod(bin2dec(X1) + bin2dec(X2),256),8);
        end
        
        %Method - subMod8
        %Description - Perform subtraction modulus 8
        
        %Parameter - X1
        %   Desc -Binary string as Input 1
        %Parameter - X2
        %   Desc -Binary string as Input 2
        %Return - DiffOut
        %   Desc - Subtract X1 and X2 and then perform modulus 8
        
        function [DiffOut] = subMod8(X1,X2)
            DiffOut = dec2bin(mod(bin2dec(X1) - bin2dec(X2),256),8);
        end
        
        
        %Method - computeBitXOr
        %Description - Compute Bitwise XOR
        
        %Parameter - inpBits1
        %   Desc -Binary string as Input 1
        %Parameter - inpBits2
        %   Desc -Binary string as Input 2
        %Return - outputBit
        %   Desc - Bitwise Xored output Bits from Input 1 and Input 2
        
        function [outputBit] = computeBitXOr(inpBits1,inpBits2)
            lenBT1 = length(inpBits1);
            lenBT2 = length(inpBits2);
            
            %Set the length of the two binary strings the same
            if(lenBT1 > lenBT2)
                inpBits2 = [dec2bin(0,lenBT1-lenBT2) inpBits2];
            elseif(lenBT2 > lenBT1)
                inpBits1 = [dec2bin(0,lenBT2-lenBT1) inpBits1];
            end
            
            %perform ascii subtraction and get the absolute value
            outputBit = sprintf('%d',abs(inpBits1-inpBits2));
            %remove space if any from the outbits
            outputBit(strfind(outputBit,' ')) = [];
            
        end
        
        %Method - roundFunc0
        %Description - Perform round function as in the specification
        %              left shift bit 1, left shift bit 2, XOR them and
        %              left shift bit 7 and XOR with the result above.
        
        %Parameter - X
        %   Desc -Binary string as input
        %Return - F0
        %   Desc - Output from the operations mentioned in description
        
        function [F0] = roundFunc0(X)
            
            %shit the input by 1 to the left
            X1 = Hight.shiftBits(X,-1);
            %shit the input by 2 to the left
            X2 = Hight.shiftBits(X,-2);
            %compute bitwise XOR from the above
            X12 = Hight.computeBitXOr(X1,X2);
            %shit the input by 7 to the left
            X7 = Hight.shiftBits(X,-7);
            F0 = Hight.computeBitXOr(X12,X7);
        end
        
        %Method - roundFunc1
        %Description - Perform round function as in the specification
        %              left shift bit 3, left shift bit 4, XOR them and
        %              left shift bit 6 and XOR with the result above.
        
        %Parameter - X
        %   Desc -Binary string as input
        %Return - F0
        %   Desc - Output from the operations mentioned in description
        
        function [F1] = roundFunc1(X)
            
            %shit the input by 3 to the left
            X3 = Hight.shiftBits(X,-3);
            %shit the input by 4 to the left
            X4 = Hight.shiftBits(X,-4);
            %compute bitwise XOR from the above
            X34 = Hight.computeBitXOr(X3,X4);
            %shit the input by 6 to the left
            X6 = Hight.shiftBits(X,-6);
            F1 = Hight.computeBitXOr(X34,X6);
        end
        
        
    end
    
    %Methods that require class instance
    methods
        
        %Method - Hight
        %Description - Constructor to set Block Size and Key Size static
        %              method call to instance variables               
        
        function Hgt = Hight()
            Hgt.BlockSize = Hight.blockSize();
            Hgt.KeySize = Hight.keySize();
        end
        
        %Hgt is the Object instance
        
        %Method - setRounds
        %Description - Set function to set the number of rounds for the
        %              block cipher
        
        %Parameter - inputRound
        %   Desc -Input integer that specifies the number of rounds        
        
        function setRounds(Hgt,inputRound)
            Hgt.round = inputRound;
        end
        
        %Method - setKey
        %Description - Set function to set the master key for the block
        %              cipher
        
        %Parameter - inputKey
        %   Desc -Input byte array that specifies the master key for the 
        %         Hight block cipher        
        
        function setKey(Hgt,inputKey)
            Hgt.key  = inputKey;
        end
        
        %Method - initialConversion
        %Description - This funciton checks for the correctness of used
        %              entered rounds and instance blocksize and key size
        %              It also converts byte array plain text and master
        %              key to Hexadecimal string
                
        %Return - PTHex
        %   Desc - Plain Text or Cipher text in Hexadecimal
        %Return - KeyHex
        %   Desc - Master Key in Hexadecimal
        
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
            
            %convert the byte array plain text or cipher text to
            %Hexadecimal String
            PTHex = dec2hex(Hgt.PT,2);
            KeyHex = dec2hex(Hgt.key,2);
            PTHex = reshape(PTHex',1,[]).'.';
            KeyHex = reshape(KeyHex',1,[]).'.';
            PTHex(isspace(PTHex)) = [];
            KeyHex(isspace(KeyHex)) = [];
            
        end
        
        %Method - encrypt
        %Description - This funciton encrypts the input plaintext using the
        %              Hight algorithm

        %Parameter - inputPT
        %   Desc -Input plaintext as byte array index 1 is Most Significant 
        %         byte and last index is Least significant byte
        
        %Return - CT
        %   Desc - Cipher text as byte array
        %Return - CTHex
        %   Desc - Cipher text in Hexadecimal
        %Return - inputPT
        %   Desc - input plain text as byte array
        %Return - PTHex
        %   Desc - input plain text in Hexadecimal
        
        function [CT,CTHex,inputPT,PTHex] = encrypt(Hgt,inputPT)
            
            %Set the plain text to the instance variable
            Hgt.PT = inputPT;
            %perfrom intial coversion
            [PTHex, KeyHex] = initialConversion(Hgt);
            %Compute the whitening key
            WK = Hight.getWhiteningKey(KeyHex);
            %Convert the key from Hexdecimal to Binary
            KeyBin = Hight.HextoBin(KeyHex);
            %generate the subkeys for the Master Key
            SK = Hight.subKeyGeneration(KeyBin);
            %convert the plain text from Hexadecimal to Binary
            PTBin =  Hight.HextoBin(PTHex);
            
            X = PTBin;
            
            %Note that 1 denotes most significant bit and 64 the least
            %significant
            
            %Intial transofrmation using the Whitening Keys WK0-3
            %Alternate Bitwise XOR and Addition Mod 2^8
            X(1:8) = X(1:8);                          %7th byte
            X(9:16) = Hight.computeBitXOr(X(9:16),WK(33:40)); %6th byte
            X(17:24) = X(17:24);                      %5th byte
            X(25:32) = Hight.addMod8(X(25:32),WK(41:48));    %4th byte
            X(33:40) = X(33:40);                      %3rd byte
            X(41:48) = Hight.computeBitXOr(X(41:48),WK(49:56)); %2nd byte
            X(49:56) = X(49:56);                          %1st byte
            X(57:64) = Hight.addMod8(X(57:64),WK(57:64));        %0th byte
            
            Xnew = X;
            
            %Perform 32 rounds of the operation as per Hight specification
            %Refer http://tools.ietf.org/html/draft-kisa-hight-00
            %Performing Round Function zero or one, Bitwise XOR, addition
            %mod 2^8 involving subkeys 0-127
            
            %Note subkey 0 is index 1 and 127 is index 128 
            %Matlab has index of array starting with 1
            
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
            
            %Swap at the end for final transformation - full round
            if(Hgt.round == 32)
                X = [X(57:64) X(1:56)];
            end
            
            %Final Transformation with whitening keys WK4-7
            Xnew(1:8) = X(1:8);
            Xnew(9:16) = Hight.computeBitXOr(X(9:16),WK(1:8));
            Xnew(17:24) = X(17:24);
            Xnew(25:32) = Hight.addMod8(X(25:32),WK(9:16));
            Xnew(33:40) = X(33:40);
            Xnew(41:48) = Hight.computeBitXOr(X(41:48),WK(17:24));
            Xnew(49:56) = X(49:56);
            Xnew(57:64) = Hight.addMod8(X(57:64),WK(25:32));
            
            %Convert Cipher text in bits to Hexadecimal
            CTHex = Hight.BintoHex(Xnew);
            %Convert Cipher text in Hexadecimal to Byte array
            CT = hex2dec(reshape(CTHex,2,[]).');
        end
        
        %Method - decrypt
        %Description - This funciton decrypts the input ciphertext using the
        %              Hight algorithm

        %Parameter - inputCT
        %   Desc -Input ciphertext as byte array index 1 is Most Significant 
        %         byte and last index is Least significant byte
        
        %Return - PT
        %   Desc - Plain text as byte array
        %Return - PTHex
        %   Desc - Plain text in Hexadecimal
        %Return - inputCT
        %   Desc - input cipher text as byte array
        %Return - PTHex
        %   Desc - input cipher text in Hexadecimal
        
        function [PT,PTHex,inputCT,CTHex] = decrypt(Hgt,inputCT)
            
            %Set the plain text to the instance variable
            Hgt.PT = inputCT;
            [CTHex, KeyHex] = initialConversion(Hgt);
            %Compute the whitening key
            WK = Hight.getWhiteningKey(KeyHex);
            %Convert the key from Hexdecimal to Binary
            KeyBin = Hight.HextoBin(KeyHex);
            %generate the subkeys for the Master Key
            SK = Hight.subKeyGeneration(KeyBin);
            %convert the cipher text from Hexadecimal to Binary
            CTBin =  Hight.HextoBin(CTHex);
            
            X = CTBin;
            
            %Note that 1 denotes most significant bit and 64 the least
            %significant
            
            %Final Transofrmation of encryption which is operaion with
            %whitening keys WK4-7
            Xnew(1:8) =X(1:8);                                %7th byte
            Xnew(9:16) = Hight.computeBitXOr(X(9:16),WK(1:8));%6th byte
            Xnew(17:24) = X(17:24);                           %5th byte      
            Xnew(25:32) = Hight.subMod8(X(25:32),WK(9:16));   %4th byte
            Xnew(33:40) = X(33:40);                           %3rd byte
            Xnew(41:48) = Hight.computeBitXOr(X(41:48),WK(17:24));%2nd byte
            Xnew(49:56) = X(49:56);                           %1st byte  
            Xnew(57:64) = Hight.subMod8(X(57:64),WK(25:32));  %0th byte
            
            X = Xnew;
            
            %Swap at the end performed during encryption  - full round
            if(Hgt.round == 32)
                X = [X(9:64) X(1:8)];
            end            
            
            %Last Iteration of encrption
            
%             Xnew(1:8) = Hight.computeBitXOr(X(1:8),Hight.addMod8...
%                 (Hight.roundFunc0(X(9:16)),SK{128}));
%             Xnew(9:16) = X(9:16);
%             Xnew(17:24) = Hight.subMod8(X(17:24),Hight.computeBitXOr...
%                 (Hight.roundFunc1(X(25:32)),SK{127}));
%             Xnew(25:32) = X(25:32);
%             Xnew(33:40) = Hight.computeBitXOr(X(33:40),Hight.addMod8...
%                 (Hight.roundFunc0(X(41:48)),SK{126}));
%             Xnew(41:48) = X(41:48);
%             Xnew(49:56) = Hight.subMod8(X(49:56),Hight.computeBitXOr...
%                 (Hight.roundFunc1(X(57:64)),SK{125}));
%             Xnew(57:64) = X(57:64);
%             
%             X = Xnew;

            %Perform 32 rounds of the operation as per Hight specification
            %Refer http://tools.ietf.org/html/draft-kisa-hight-00
            %Performing Round Function zero or one, Bitwise XOR, addition
            %mod 2^8 subtraction mod 2^8 involving subkeys 0-127
            
            %Note subkey 0 is index 1 and 127 is index 128 
            %Matlab has index of array starting with 1
            
            %The final stage addition Mod 2^8 is replaced to Subtraction
            %mod 2^8. The addition mod 2^8 involving subkey will remain the
            %same as in encryption process
            
            for incr1 = Hgt.round:-1:1
                
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
            
            %First iteration of decryption with whitening keys WK0-3
            
            X(1:8) = X(1:8);                          %7
            X(9:16) = Hight.computeBitXOr(X(9:16),WK(33:40)); %6
            X(17:24) = X(17:24);                      %5
            X(25:32) = Hight.subMod8(X(25:32),WK(41:48));    %4
            X(33:40) = X(33:40);                      %3
            X(41:48) = Hight.computeBitXOr(X(41:48),WK(49:56)); %2
            X(49:56) = X(49:56);                          %1
            X(57:64) = Hight.subMod8(X(57:64),WK(57:64));        %0
            
            Xnew = X;
            
            %Convert Plain text in bits to Hexadecimal
            PTHex = Hight.BintoHex(Xnew);
            %Convert Plain text in Hexadecimal to Byte array
            PT = hex2dec(reshape(PTHex,2,[]).');
        end                
    end
end