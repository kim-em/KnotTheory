package org.katlas.JavaKh;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;

public class CheckKomplex {

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

  private boolean checkComplex(File file) {
    ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file));
    return checkComplex(ois);
  }

  private boolean checkComplex(ObjectInputStream ois) {
    
  }
  
}
