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
    public static final String usage = "java hightTest <key>"; 
    /*
     * Test program takes a hexadecimal key and prints out test information 
     * using that key.
     */
    public static void main(String args[]){
        byte[] key = new byte[16];

        if(args.length < 1){
            System.err.println(usage);
            System.exit(1);
        }
        
        try{
            key = DatatypeConverter.parseHexBinary(args[0]);
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
        String[] strs = bytesToString(key);
        
        System.out.println("Key bytes: ");
        for(String str : strs){
            System.out.println(str);
        }

        // System.out.println("Whitening keys: ");
        // strs = bytesToString(testWK(key));
        // for(String str : strs){
        //     System.out.println(str);
        // }

        // Uncomment testLFSR and make generateLFSRConsts public in HIGHT
        // to test.
         strs = bytesToString(testLFSR(key));
         System.out.println("LFSR constants: ");
         for(String str : strs){
             System.out.println(str);
         }

    }
    
    /**
     * This method creates a HIGHT object and then calculates the LFSR constants.
     * This is not dependent on key.
     *
     *@param key The key to give to the HIGHT object.
     *@return byte[] The LFSR state bytes.
     */
     public static byte[] testLFSR(byte[] key){
         HIGHT h = new HIGHT(key); 
         return h.generateLFSRConsts();
     }

    /**
     * This method creates a HIGHT object and then calculates its whitening keys,
     * returning them. 
     *
     * @param key A byte array of the master key.
     * @return byte[] The resulting whitening keys.
     */
    public static byte[] testWK(byte[] key){
        HIGHT h = new HIGHT(key);
        return HIGHT.generateWhiteningKeys(key);
    }

    /**
     * This method will return an array of bytes represented as strings.
     *
     * @param bytes The bytes to convert.
     * @return String[] The array of converted strings
     */
    public static String[] bytesToString(byte[] bytes){
       String[] strs = new String[bytes.length]; 

        for(int i = 0; i < bytes.length; i++){
            // Should eventually come up with constants for these masks
            strs[i] = "" + ((short)bytes[i] & 0x00ff);
        }

        return strs;
    }
}

