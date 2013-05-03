package etomo.ui.swing;

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.ToolTipManager;

import etomo.BaseManager;
import etomo.logic.DirectiveEditorBuilder;
import etomo.logic.DirectiveTool;
import etomo.storage.DirectiveDescrSection;
import etomo.storage.DirectiveMap;
import etomo.type.AxisType;
import etomo.type.DialogType;
import etomo.type.DirectiveFileType;
import etomo.ui.DirectiveDisplaySettings;
import etomo.ui.FieldType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class DirectiveEditorDialog implements Expandable, DirectiveDisplaySettings {
  public static final String rcsid = "$Id:$";

  private final CheckBox[] cbInclude = new CheckBox[DirectiveFileType.NUM];
  private final CheckBox[] cbExclude = new CheckBox[DirectiveFileType.NUM];
  private final CheckBox cbShowHidden = new CheckBox("Show hidden");
  private final CheckBox cbShowUnchanged = new CheckBox("Show unchanged");
  private final JPanel pnlConfigBody = new JPanel();
  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlSourceBody = new JPanel();
  private final List<DirectiveSectionPanel> sectionArray = new ArrayList<DirectiveSectionPanel>();

  private final BaseManager manager;
  private final PanelHeader phConfig;
  private final PanelHeader phSource;
  private final DirectiveTool tool;
  private final boolean[] fileTypeExists;

  private DirectiveEditorDialog(final BaseManager manager, final DirectiveFileType type,
      final DirectiveEditorBuilder builder) {
    this.manager = manager;
    phConfig = PanelHeader.getInstance("Configure Editor", this,
        DialogType.DIRECTIVE_EDITOR);
    phSource = PanelHeader.getInstance("Source", this, DialogType.DIRECTIVE_EDITOR);
    fileTypeExists = builder.getFileTypeExists();
    if (type == null) {
      tool = new DirectiveTool(null, false, this);
    }
    else {
      tool = new DirectiveTool(type, fileTypeExists[type.getIndex()], this);
    }
  }

  public static DirectiveEditorDialog getInstance(final BaseManager manager,
      final DirectiveFileType type, final DirectiveEditorBuilder builder,
      final AxisType sourceAxisType, final String sourceStatus,
      final String saveTimestamp, final StringBuffer errmsg) {
    DirectiveEditorDialog instance = new DirectiveEditorDialog(manager, type, builder);
    instance.createPanel(type, builder, sourceAxisType, sourceStatus, saveTimestamp,
        errmsg);
    instance.addListeners();
    return instance;
  }

  private void createPanel(final DirectiveFileType type,
      final DirectiveEditorBuilder builder, final AxisType sourceAxisType,
      final String sourceStatus, final String saveTimestamp, StringBuffer errmsg) {
    // construct
    int nColumns = 3;
    JScrollPane scrollPane = new JScrollPane(pnlRoot);
    JPanel pnlConfig = new JPanel();
    JPanel pnlShow = new JPanel();
    JPanel pnlSource = new JPanel();
    JPanel pnlIncludeCheckboxes = new JPanel();
    JPanel pnlShowGlue = new JPanel();
    JPanel pnlIncludeSettings = new JPanel();
    JPanel pnlSections = new JPanel();
    JPanel pnlSectionColumns[] = new JPanel[nColumns];
    LabeledTextField ltfSource = new LabeledTextField(FieldType.STRING, "Dataset: ");
    LabeledTextField ltfTimestamp = new LabeledTextField(FieldType.STRING, "Saved: ");
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      cbInclude[i] = new CheckBox();
      cbInclude[i].setActionCommand(DirectiveFileType.getLabel(i));
      cbExclude[i] = new CheckBox();
      cbExclude[i].setActionCommand(DirectiveFileType.getLabel(i));
      // init
      if (!fileTypeExists[i]) {
        cbInclude[i].setEnabled(false);
        cbExclude[i].setEnabled(false);
      }
      else if (type != null && i == type.getIndex()) {
        cbInclude[i].setSelected(true);
      }
      else {
        cbExclude[i].setSelected(true);
      }
    }
    // init
    ltfSource.setEditable(false);
    ltfSource.setText(sourceStatus);
    ltfTimestamp.setEditable(false);
    ltfTimestamp.setText(saveTimestamp);
    // Fill sectionArray
    Iterator<DirectiveDescrSection> descrIterator = builder.getSectionArray().iterator();
    final DirectiveMap directiveMap = builder.getDirectiveMap();
    while (descrIterator.hasNext()) {
      DirectiveDescrSection descrSection = descrIterator.next();
      if (descrSection.isContainsEditableDirectives()) {
        DirectiveSectionPanel section = DirectiveSectionPanel.getInstance(manager,
            descrSection, directiveMap, sourceAxisType, tool, this);
        sectionArray.add(section);
      }
    }
    // root panel
    ToolTipManager.sharedInstance().registerComponent(pnlRoot);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlSource);
    pnlRoot.add(pnlConfig);
    pnlRoot.add(pnlSections);
    // source panel
    pnlSource.setLayout(new BoxLayout(pnlSource, BoxLayout.Y_AXIS));
    pnlSource.setBorder(BorderFactory.createEtchedBorder());
    pnlSource.add(phSource.getContainer());
    pnlSource.add(pnlSourceBody);
    // source body panel
    pnlSourceBody.setLayout(new BoxLayout(pnlSourceBody, BoxLayout.Y_AXIS));
    pnlSourceBody.add(ltfSource.getComponent());
    pnlSourceBody.add(ltfTimestamp.getComponent());
    // config panel
    pnlConfig.setLayout(new BoxLayout(pnlConfig, BoxLayout.Y_AXIS));
    pnlConfig.setBorder(BorderFactory.createEtchedBorder());
    pnlConfig.add(phConfig.getContainer());
    pnlConfig.add(pnlConfigBody);
    // config body panel
    pnlConfigBody.setLayout(new BoxLayout(pnlConfigBody, BoxLayout.X_AXIS));
    pnlConfigBody.add(Box.createHorizontalGlue());
    pnlConfigBody.add(pnlIncludeSettings);
    pnlConfigBody.add(Box.createHorizontalGlue());
    pnlConfigBody.add(pnlShowGlue);
    pnlConfigBody.add(Box.createHorizontalGlue());
    // include panel
    pnlIncludeSettings.setLayout(new BoxLayout(pnlIncludeSettings, BoxLayout.Y_AXIS));
    pnlIncludeSettings.setBorder(new EtchedBorder("Include Directives").getBorder());
    pnlIncludeSettings.add(pnlIncludeCheckboxes);
    // include checkboxes panel
    pnlIncludeCheckboxes.setLayout(new GridLayout(0, 3));
    pnlIncludeCheckboxes.add(new JLabel("Include"));
    pnlIncludeCheckboxes.add(new JLabel("Exclude"));
    pnlIncludeCheckboxes.add(new JLabel("File"));
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      pnlIncludeCheckboxes.add(cbInclude[i]);
      pnlIncludeCheckboxes.add(cbExclude[i]);
      pnlIncludeCheckboxes.add(new JLabel(DirectiveFileType.toString(i)));
    }
    // show glue panel
    pnlShowGlue.setLayout(new BoxLayout(pnlShowGlue, BoxLayout.Y_AXIS));
    pnlShowGlue.add(pnlShow);
    pnlShowGlue.add(Box.createVerticalGlue());
    // show panel
    pnlShow.setLayout(new BoxLayout(pnlShow, BoxLayout.Y_AXIS));
    pnlShow.setBorder(new EtchedBorder("Show Directives").getBorder());
    pnlShow.add(cbShowUnchanged);
    pnlShow.add(cbShowHidden);
    // sections
    pnlSections.setLayout(new BoxLayout(pnlSections, BoxLayout.X_AXIS));
    // columns
    int nSections = sectionArray.size();
    int nRows = nSections / nColumns;
    int remainder = nSections % nColumns;
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    List<DirectiveSectionPanel> columnSectionArray = new ArrayList<DirectiveSectionPanel>();
    for (int i = 0; i < nColumns; i++) {
      //Build the columns.
      pnlSectionColumns[i] = new JPanel();
      pnlSectionColumns[i]
          .setLayout(new BoxLayout(pnlSectionColumns[i], BoxLayout.Y_AXIS));
      pnlSections.add(pnlSectionColumns[i]);
      if (i < nColumns - 1) {
        pnlSections.add(Box.createRigidArea(FixedDim.x5_y0));
        pnlSections.add(new JSeparator(SwingConstants.VERTICAL));
        pnlSections.add(Box.createRigidArea(FixedDim.x5_y0));
      }
      //Add the checkboxes and tempoarily store the sections in this column.
      columnSectionArray.clear();
      int extraRow = remainder-- > 0 ? 1 : 0;
      for (int j = 0; j < nRows + extraRow; j++) {
        DirectiveSectionPanel sectionPanel = null;
        if (iterator.hasNext()) {
          sectionPanel = iterator.next();
        }
        if (sectionPanel == null) {
          break;
        }
        columnSectionArray.add(sectionPanel);
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(sectionPanel.getShowCheckBox());
        panel.add(Box.createHorizontalGlue());
        pnlSectionColumns[i].add(panel);
      }
      //Put padding between the chckboxes and the section panels.
      if (extraRow == 0) {
        pnlSectionColumns[i].add(Box.createRigidArea(FixedDim.x0_y20));
      }
      else {
        pnlSectionColumns[i].add(Box.createRigidArea(FixedDim.x0_y5));
      }
      //Add the section panels.
      Iterator<DirectiveSectionPanel> columnIterator = columnSectionArray.iterator();
      while (columnIterator.hasNext()) {
        pnlSectionColumns[i].add(columnIterator.next().getComponent());
      }
      pnlSectionColumns[i].add(Box.createVerticalGlue());
    }
    if (errmsg != null && errmsg.length() > 0) {
      UIHarness.INSTANCE.openMessageDialog(manager, errmsg.toString(),
          "Problems Building Editor");
    }
  }

  private void addListeners() {
    DirectiveListener listener = new DirectiveListener(this);
    cbShowUnchanged.addActionListener(listener);
    cbShowHidden.addActionListener(listener);
    IncludeListener includeListener = new IncludeListener(this);
    ExcludeListener excludeListener = new ExcludeListener(this);
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      cbInclude[i].addActionListener(includeListener);
      cbExclude[i].addActionListener(excludeListener);
    }
  }

  public Container getContainer() {
    return pnlRoot;
  }

  public boolean isInclude(final int index) {
    if (index >= 0 && index < DirectiveFileType.NUM) {
      return cbInclude[index].isSelected();
    }
    return false;
  }

  public boolean isExclude(final int index) {
    if (index >= 0 && index < DirectiveFileType.NUM) {
      return cbExclude[index].isSelected();
    }
    return false;
  }

  public boolean isShowUnchanged() {
    return cbShowUnchanged.isSelected();
  }

  public boolean isShowHidden() {
    return cbShowHidden.isSelected();
  }

  private void action() {
    updateDisplay(false);
  }

  private void updateDisplay(final boolean showSection) {
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    while (iterator.hasNext()) {
      iterator.next().updateDisplay();
    }
    UIHarness.INSTANCE.pack(manager);
  }

  void showSection() {
    updateDisplay(true);
  }

  private void includeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      if (cbInclude[index].isSelected() || cbExclude[index].isSelected()) {
        cbExclude[index].setSelected(false);
      }
    }
    updateDisplay(false);
  }

  private void excludeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      if (cbExclude[index].isSelected() || cbInclude[index].isSelected()) {
        cbInclude[index].setSelected(false);
      }
    }
    updateDisplay(false);
  }

  public void expand(final ExpandButton button) {
    if (phConfig.equalsOpenClose(button)) {
      pnlConfigBody.setVisible(button.isExpanded());
    }
    if (phSource.equalsOpenClose(button)) {
      pnlSourceBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(manager);
  }

  public final void expand(final GlobalExpandButton button) {
  }

  private static final class DirectiveListener implements ActionListener {
    private final DirectiveEditorDialog dialog;

    private DirectiveListener(final DirectiveEditorDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action();
    }
  }

  private static final class IncludeListener implements ActionListener {
    private final DirectiveEditorDialog dialog;

    private IncludeListener(final DirectiveEditorDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.includeAction(event.getActionCommand());
    }
  }

  private static final class ExcludeListener implements ActionListener {
    private final DirectiveEditorDialog dialog;

    private ExcludeListener(final DirectiveEditorDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.excludeAction(event.getActionCommand());
    }
  }
}
