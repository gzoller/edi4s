package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public interface Difference {
    Path getPath();
    String getName();
    String getCanonicalName();
    SimpleEntry<Boolean, Boolean> getPresence();
    SimpleEntry<Boolean, Boolean> getRequired();

    default boolean isOk() {
        SimpleEntry<Boolean, Boolean> p = getPresence();
        SimpleEntry<Boolean, Boolean> r = getRequired();
        return p.getKey().equals(p.getValue())
                && (r.getKey().equals(r.getValue()) || r.getKey());
    }

    List<BodyRow> render(int nestLevel);
}