/*
 * Authors: Christopher Sasarak
 *          Srinivas Sridharan
 */

/**
 * This class provides an implementation of the
 * HIGHT block cipher. 
*/ 
public class HIGHT implements BlockCipher{
    public static final int BLOCK_SIZE = 128, KEY_SIZE = 64, 
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
        this.key = key;
        this.rounds = rounds;
    }
    
    /**
     * This constructor takes no arguments and will set the HIGHT
     * object to use the default values described in the HIGHT paper.
     * NOTE: The key needs to be specified separately with the setKey method.
     */
    public HIGHT(){
        this(null, HIGHT.DEFAULT_ROUNDS);
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
        // TODO: Set the whitening keys as well when a new key is set
        this.key = key;   
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
