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
    public static final String usage = "java hightTest <key> <plaintext>"; 
    /*
     * Test program takes a hexadecimal key and prints out test information 
     * using that key.
     */
    public static void main(String args[]){
        byte[] key = new byte[16];
        byte[] plaintext = new byte[8];
        HIGHT h;
        String[] strs;

        if(args.length < 2){
            System.err.println(usage);
            System.exit(1);
        }
        
        try{
            key = DatatypeConverter.parseHexBinary(args[0]);
            plaintext = DatatypeConverter.parseHexBinary(args[1]);
        }
        catch(NumberFormatException e){
            System.err.println(e.getMessage());
            System.exit(1);
        }
        catch(IllegalArgumentException e){
            System.err.println(e.getMessage());
            System.exit(1);
        }
        
        h = new HIGHT(key);
        
        System.out.println("Key bytes: ");
        h.setKey(key);
        System.out.println(DatatypeConverter.printHexBinary(h.key));

        System.out.println("Whitening keys: ");
        System.out.println(DatatypeConverter.printHexBinary(h.wKeys));

        System.out.println("Subkeys: " );
        for(int i = 3; i < 128; i += 4){ System.out.printf("skeys %d-%d: %02x%02x%02x%02x\n", 
                i, i - 3, h.subkeys[i], h.subkeys[i-1],
                h.subkeys[i-2], h.subkeys[i-3]);
        }

        System.out.println("Initial transform: ");
        System.out.println(DatatypeConverter.printHexBinary(h.initialTransform(plaintext)));
    }

    
}

