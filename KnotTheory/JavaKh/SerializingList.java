import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Iterator;
import java.util.List;


public interface SerializingList<E extends Serializable> extends List<E> {

	public Iterator<ObjectInputStream> getSerializedForms() throws IOException;
	
}
