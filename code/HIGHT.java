/*
 * Authors: Christopher Sasarak
 *          Srinivas Sridharan
 */

/**
 * This class provides an implementation of the
 * HIGHT block cipher. 
*/ 
public class HIGHT implements BlockCipher{
    public static final int BLOCK_SIZE = 128, KEY_SIZE = 128, 
        DEFAULT_ROUNDS = 32;

    private int rounds, keySize, blockSize;
    private byte[] key, wKeys;

    /**
     * This constructor will intialize this HIGHT object
     * with the properties given in its parameters.
     *
     * @param key The key to use to encrypt the cipher-text with.
     * @param rounds The number of rounds to run. 
     */
    public HIGHT(byte[] key, int rounds){
        this.setKey(key);
        this.rounds = rounds;
    }
    
    /**
     * This constructor only a key and will set the HIGHT
     * object to use the default number of rounds as described in the 
     * HIGHT paper.
     *
     * @param key A byte array of the 128-bit master key.
     */
    public HIGHT(byte[] key){
        this(key, HIGHT.DEFAULT_ROUNDS);
    }

    /**
     * This function generates whitening keys from the input master key.
     *
     * @param mk The master key as an array of bytes.
     * @return byte[] An array of whitening keys. 
     */
    // TODO: After testing, we should be able to make this private.
    public byte[] generateWhiteningKeys(byte[] mk) {
        byte[] wk = new byte[8];

        for(int i = 0; i < 8; i++){
            if(i >= 0 && i <= 3)
                wk[i] = mk[i+12];
            else
                wk[i] = mk[i-4]; 
        }

        return wk;
    }
    

    /**
	 * Returns this block cipher's block size in bytes.
	 *
	 * @return  Block size.
	 */
	public int blockSize(){
        return HIGHT.BLOCK_SIZE;
    }

	/**
	 * Returns this block cipher's key size in bytes.
	 *
	 * @return  Key size.
	 */
	public int keySize(){
        return this.key.length;
    }

	/**
	 * Set the number of rounds to use in this block cipher's algorithm. If
	 * <TT>setRounds()</TT> is not called, the official number of rounds stated
	 * in the algorithm specification is used.
	 *
	 * @param  R  Number of rounds.
	 */
	public void setRounds(int R){
        this.rounds = R;
    }

	/**
	 * Set the key for this block cipher. <TT>key</TT> must be an array of bytes
	 * whose length is equal to <TT>keySize()</TT>.
	 *
	 * @param  key  Key.
	 */
	public void setKey(byte[] key){
        this.key = key;   
        this.wKeys = this.generateWhiteningKeys(this.key);
    }

	/**
	 * Encrypt the given plaintext. <TT>text</TT> must be an array of bytes
	 * whose length is equal to <TT>blockSize()</TT>. On input, <TT>text</TT>
	 * contains the plaintext block. The plaintext block is encrypted using the
	 * key specified in the most recent call to <TT>setKey()</TT>. On output,
	 * <TT>text</TT> contains the ciphertext block.
	 *
	 * @param  text  Plaintext (on input), ciphertext (on output).
	 */
	public void encrypt
		(byte[] text){}

}
