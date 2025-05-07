package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public interface SegmentDifference extends Difference {
    Optional<SimpleEntry<List<String>, List<String>>> getAssertions();
    List<FieldDifference> getFieldDiff();

    default List<BodyRow> render(int nestLevel) {
        String rationalName = getName().equals(getCanonicalName())
                ? getName()
                : getCanonicalName() + " (" + getName() + ")";
        String req1 = getRequired().getKey() ? "required" : "optional";
        String req2 = getRequired().getValue() ? "required" : "optional";
        Row presenceRow = DifferenceModels.presenceRow(
                rationalName,
                nestLevel,
                getPresence(),
                getRequired(),
                Style.SECONDARY
        );
        boolean muted = DifferenceModels.isMuted(presenceRow);
        List<BodyRow> rows = new ArrayList<>();
        rows.add(presenceRow);
        if (!muted) {
            getFieldDiff().forEach(fd -> rows.addAll(fd.render(nestLevel + 1)));
        }
        return rows;
    }
}