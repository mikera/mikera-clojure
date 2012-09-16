package mikera.clojure;
import java.io.IOException;


import clojure.lang.*;

public class ClojureRunner {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			RT.loadResourceScript("mc/util.clj");
			System.out.println(clojure.lang.RT.var("mc.util","middle").invoke(1,2,3));
		} catch (IOException e) {
			throw new ClojureError(e);
		}
	}

}
