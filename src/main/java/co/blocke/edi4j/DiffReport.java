package co.blocke.edi4j;

import co.blocke.edi4j.model4j.difference.*;
import co.blocke.edi4j.model4j.refined.RefinedDocumentSpec;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Builds a Table-based EDI diff report.
 */
public class DiffReport {

    /**
     * Generate a diff report table comparing src vs target through the EDI standard.
     * @param srcPartner      name of source partner
     * @param targetPartner   name of target partner
     * @param src             source document spec
     * @param edi             EDI standard document spec
     * @param target          target document spec
     * @param filterUnused    if true, drop rows where all cells are muted
     */
    public static Table genReport(
            String srcPartner,
            String targetPartner,
            RefinedDocumentSpec src,
            RefinedDocumentSpec edi,
            RefinedDocumentSpec target,
            boolean filterUnused
    ) {
        // 1) Compute differences
        List<Difference> diffs = DiffEngine.compareSpecs(src, edi, target);

        // 2) Titles
        List<Title> titles = List.of(
                new Title(List.of(new Cell("📦 EDI Segment Comparison Report"))),
                new Title(List.of(new Cell(srcPartner + " -to- " + targetPartner)))
        );

        // 3) Header row
        Header header = new Header(List.of(
                new Cell("Source (" + srcPartner + ")"),
                new Cell("Difference"),
                new Cell("Target (" + targetPartner + ")"),
                new Cell("Difference")
        ));

        // 4) Flattened body rows
        List<BodyRow> rawRows = new ArrayList<>();
        for (Difference diff : diffs) {
            // diff.render(0) produces a List<BodyRow>
            rawRows.addAll(diff.render(0));
        }

        // 5) Optionally filter out fully-muted rows
        List<BodyRow> rows;
        if (filterUnused) {
            rows = rawRows.stream()
                    .filter(row -> row.getCells().stream()
                            .anyMatch(cell -> cell.getStyle().filter(s -> s != Style.MUTED).isPresent())
                    )
                    .collect(Collectors.toList());
        } else {
            rows = rawRows;
        }

        // 6) Build table (4 columns, widths 35/15/35/15%, total width 200)
        return new Table(
                titles,
                4,
                List.of(35, 15, 35, 15),
                200,
                header,
                rows
        );
    }

    /**
     * Overload with filterUnused defaulting to true.
     */
    public static Table genReport(
            String srcPartner,
            String targetPartner,
            RefinedDocumentSpec src,
            RefinedDocumentSpec edi,
            RefinedDocumentSpec target
    ) {
        return genReport(srcPartner, targetPartner, src, edi, target, true);
    }
}
