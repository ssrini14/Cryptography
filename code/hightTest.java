/*
 * Authors: Christopher Sasarak
 *          Srinivas Sridharan
 */

import javax.xml.bind.DatatypeConverter;
/**
 * This class provides a main function that we can use
 * to test the hight algorithm.
 */

public class hightTest{
    public static final String usage = "java hightTest <keySize> <key>"; 
    /*
     * Test program takes a keysize (in bits) and a key and
     * prints the byte array made from that key.
     */
    public static void main(String args[]){
        byte[] bs = null;
        int keySize = 0;

        if(args.length < 2){
            System.err.println(usage);
            System.exit(1);
        }
        
        try{
            keySize = Integer.parseInt(args[0]);
            bs = new byte[keySize / 8];
            
            // Might need to add 1 if the keySize % 8 != 0
            bs = DatatypeConverter.parseHexBinary(args[1]);
        }
        catch(NumberFormatException e){
            System.err.println(e.getMessage());
            System.exit(1);
        }
        catch(IllegalArgumentException e){
            System.err.println(e.getMessage());
            System.exit(1);
        }
        

        // DO THINGS THAT ARE INTERESTING HERE!
        String[] strs = convertBytes(bs);

        for(String str : strs){
            System.out.println(str);
        }
    }

    /**
     * This method will return an array of bytes represented as strings.
     *
     * @param bytes The bytes to convert.
     * @return String[] The array of converted strings
     */
    public static String[] convertBytes(byte[] bytes){
       String[] strs = new String[bytes.length]; 

        for(int i = 0; i < bytes.length; i++){
            // Should eventually come up with constants for these masks
            strs[i] = "" + ((int)bytes[i] & 0x0000ff);
        }

        return strs;
    }
}

