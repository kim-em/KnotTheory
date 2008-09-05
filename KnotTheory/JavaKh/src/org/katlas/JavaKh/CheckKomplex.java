package org.katlas.JavaKh;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.utils.LimitedSizeInputStream;
import org.katlas.JavaKh.utils.SerializingList;

public class CheckKomplex {
  private static final Log             log                        = LogFactory.getLog(CheckKomplex.class);

   public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

  private boolean checkComplex(File file) throws FileNotFoundException, IOException {
    ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file));
    return checkComplex(ois);
  }

  private boolean checkComplex(ObjectInputStream s) throws IOException {
    int serializationVersion = s.readInt();
    if(serializationVersion != 2) {
      log.warn("Unknown serialization version:" + serializationVersion);
    }
    
    int size = s.readInt();
    int ncolumns = size + 1;
    int startnum = s.readInt();
    SmoothingColumn columns[] = new SmoothingColumn[ncolumns];
    for (int i = 0; i < ncolumns; ++i) {
      columns[i] = (SmoothingColumn) s.readObject();
    }
    boolean inMemory = !s.readBoolean();
    if (!inMemory) {
      for (int i = 0; i < size; ++i) {
        log.debug("Reading height " + (i + 1) + "/" + size);
        long fileLength = s.readLong();
        int hash = s.readInt();
        InputStream lsis = new LimitedSizeInputStream(s, fileLength);
        ObjectInputStream p = new ObjectInputStream(lsis);
        matrices.add((CobMatrix<R>) (p.readObject()));
        }
      }
    } else {
      for (int i = 0; i < size; ++i) {
        debug("Reading height " + (i + 1) + "/" + size);
        matrices.add((CobMatrix<R>) (s.readObject()));
      }
    
    return false;
  }
  
    private CobMatrix<R> readNextMatrix(InputStream s) {
      
    }
}
