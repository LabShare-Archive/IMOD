package etomo.ui.swing;

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.ToolTipManager;

import etomo.BaseManager;
import etomo.logic.DirectiveEditorBuilder;
import etomo.logic.DirectiveTool;
import etomo.storage.AutodocFilter;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrSection;
import etomo.storage.DirectiveMap;
import etomo.type.AxisID;
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
I* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class DirectiveEditorDialog implements Expandable, DirectiveDisplaySettings {
  public static final String rcsid = "$Id:$";

  private final CheckBox[] cbInclude = new CheckBox[DirectiveFileType.NUM];
  private final CheckBox[] cbExclude = new CheckBox[DirectiveFileType.NUM];
  private final CheckBox cbShowHidden = new CheckBox("Show hidden");
  private final CheckBox cbShowUnchanged = new CheckBox("Show unchanged");
  private final CheckBox cbShowOnlyIncluded = new CheckBox("Show only included");
  private final JPanel pnlControlBody = new JPanel();
  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlSourceBody = new JPanel();
  private final List<DirectiveSectionPanel> sectionArray = new ArrayList<DirectiveSectionPanel>();
  private final MultiLineButton btnCloseAll = new MultiLineButton("Close All Sections");
  private final MultiLineButton btnSave = new MultiLineButton("Save");
  private final MultiLineButton btnCancel = new MultiLineButton("Cancel");
  private final LabeledTextField ltfSource = new LabeledTextField(FieldType.STRING,
      "Dataset: ");
  private final LabeledTextField ltfFileTimestamp = new LabeledTextField(
      FieldType.STRING, "File saved at: ");
  private final LabeledTextField ltfDatasetTimestamp = new LabeledTextField(
      FieldType.STRING, "Dataset saved at: ");
  private File lastFileChooserLocation = null;

  private final BaseManager manager;
  private final PanelHeader phControl;
  private final PanelHeader phSource;
  private final DirectiveTool tool;
  private final boolean[] fileTypeExists;
  private final DirectiveEditorBuilder builder;
  private final DirectiveFileType type;

  private DirectiveEditorDialog(final BaseManager manager, final DirectiveFileType type,
      final DirectiveEditorBuilder builder) {
    this.manager = manager;
    this.builder = builder;
    this.type = type;
    phControl = PanelHeader.getInstance("Control Panel", this,
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
    instance.createPanel(sourceAxisType, sourceStatus, saveTimestamp, errmsg);
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel(final AxisType sourceAxisType, final String sourceStatus,
      final String saveTimestamp, StringBuffer errmsg) {
    // construct
    int nColumns = 3;
    JScrollPane scrollPane = new JScrollPane(pnlRoot);
    JPanel pnlControl = new JPanel();
    JPanel pnlShowCheckboxes = new JPanel();
    JPanel pnlSource = new JPanel();
    JPanel pnlIncludeCheckboxes = new JPanel();
    JPanel pnlShow = new JPanel();
    JPanel pnlIncludeSettings = new JPanel();
    JPanel pnlSections = new JPanel();
    JPanel pnlSectionColumns[] = new JPanel[nColumns];
    JPanel pnlButtons = new JPanel();
    JLabel[] lInclude = new JLabel[DirectiveFileType.NUM];
    int index = -1;
    if (type != null) {
      index = type.getIndex();
    }
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      cbInclude[i] = new CheckBox();
      cbInclude[i].setActionCommand(DirectiveFileType.getLabel(i));
      cbExclude[i] = new CheckBox();
      cbExclude[i].setActionCommand(DirectiveFileType.getLabel(i));
      lInclude[i] = new JLabel(DirectiveFileType.toString(i));
      // init
      if (!fileTypeExists[i]) {
        cbInclude[i].setEnabled(false);
        cbExclude[i].setEnabled(false);
        lInclude[i].setEnabled(false);
      }
      else if (i == index) {
        // The matching file type should be included
        cbInclude[i].setSelected(true);
      }
      else {
        // Lower priority file types should be excluded. This class is indexed in order of
        // priority.
        if (i < index) {
          cbExclude[i].setSelected(true);
        }
      }
    }
    // init
    if (type == DirectiveFileType.BATCH) {
      cbShowHidden.setEnabled(false);
    }
    ltfSource.setEditable(false);
    ltfSource.setText(sourceStatus);
    ltfDatasetTimestamp.setEditable(false);
    ltfDatasetTimestamp.setText(saveTimestamp);
    ltfFileTimestamp.setEditable(false);
    btnCloseAll.setSingleLineSize();
    btnSave.setSize();
    btnCancel.setSize();
    // Fill sectionArray
    Iterator<DirectiveDescrSection> descrIterator = builder.getSectionArray().iterator();
    final DirectiveMap directiveMap = builder.getDirectiveMap();
    while (descrIterator.hasNext()) {
      DirectiveDescrSection descrSection = descrIterator.next();
      if (descrSection.isContainsEditableDirectives()) {
        DirectiveSectionPanel section = DirectiveSectionPanel.getInstance(manager,
            descrSection, directiveMap, sourceAxisType, tool);
        sectionArray.add(section);
      }
    }
    // root panel
    ToolTipManager.sharedInstance().registerComponent(pnlRoot);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlSource);
    pnlRoot.add(pnlControl);
    pnlRoot.add(pnlSections);
    // source panel
    pnlSource.setLayout(new BoxLayout(pnlSource, BoxLayout.Y_AXIS));
    pnlSource.setBorder(BorderFactory.createEtchedBorder());
    pnlSource.add(phSource.getContainer());
    pnlSource.add(pnlSourceBody);
    // source body panel
    pnlSourceBody.setLayout(new BoxLayout(pnlSourceBody, BoxLayout.Y_AXIS));
    pnlSourceBody.add(ltfSource.getComponent());
    pnlSourceBody.add(ltfDatasetTimestamp.getComponent());
    pnlSourceBody.add(ltfFileTimestamp.getComponent());
    // control panel
    pnlControl.setLayout(new BoxLayout(pnlControl, BoxLayout.Y_AXIS));
    pnlControl.setBorder(BorderFactory.createEtchedBorder());
    pnlControl.add(phControl.getContainer());
    pnlControl.add(pnlControlBody);
    // control body panel
    pnlControlBody.setLayout(new BoxLayout(pnlControlBody, BoxLayout.X_AXIS));
    pnlControlBody.add(pnlIncludeSettings);
    pnlControlBody.add(Box.createHorizontalGlue());
    pnlControlBody.add(pnlShow);
    pnlControlBody.add(Box.createHorizontalGlue());
    pnlControlBody.add(pnlButtons);
    // include panel
    // include settings
    pnlIncludeSettings.setLayout(new BoxLayout(pnlIncludeSettings, BoxLayout.Y_AXIS));
    pnlIncludeSettings.setBorder(new EtchedBorder("Include Based on Directives in Files")
        .getBorder());
    pnlIncludeSettings.add(pnlIncludeCheckboxes);
    // include checkboxes panel
    pnlIncludeCheckboxes.setLayout(new GridLayout(0, 3));
    pnlIncludeCheckboxes.add(new JLabel("Include"));
    pnlIncludeCheckboxes.add(new JLabel("Exclude"));
    pnlIncludeCheckboxes.add(new JLabel("File"));
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      pnlIncludeCheckboxes.add(cbInclude[i]);
      pnlIncludeCheckboxes.add(cbExclude[i]);
      pnlIncludeCheckboxes.add(lInclude[i]);
    }
    // show glue panel
    pnlShow.setLayout(new BoxLayout(pnlShow, BoxLayout.Y_AXIS));
    pnlShow.add(pnlShowCheckboxes);
    pnlShow.add(Box.createRigidArea(FixedDim.x0_y2));
    pnlShow.add(btnCloseAll.getComponent());
    pnlShow.add(Box.createRigidArea(FixedDim.x0_y2));
    pnlShow.add(Box.createVerticalGlue());
    // show panel
    pnlShowCheckboxes.setLayout(new BoxLayout(pnlShowCheckboxes, BoxLayout.Y_AXIS));
    pnlShowCheckboxes.setBorder(new EtchedBorder("Show Directives").getBorder());
    pnlShowCheckboxes.add(cbShowUnchanged);
    pnlShowCheckboxes.add(cbShowHidden);
    pnlShowCheckboxes.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlShowCheckboxes.add(cbShowOnlyIncluded);
    pnlShowCheckboxes.add(Box.createRigidArea(FixedDim.x0_y2));
    // Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.Y_AXIS));
    pnlButtons.add(Box.createVerticalGlue());
    pnlButtons.add(btnSave.getComponent());
    pnlButtons.add(Box.createVerticalGlue());
    pnlButtons.add(btnCancel.getComponent());
    pnlButtons.add(Box.createVerticalGlue());
    // sections
    pnlSections.setLayout(new BoxLayout(pnlSections, BoxLayout.X_AXIS));
    // columns
    int nSections = sectionArray.size();
    int nRows = nSections / nColumns;
    int remainder = nSections % nColumns;
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    List<DirectiveSectionPanel> columnSectionArray = new ArrayList<DirectiveSectionPanel>();
    for (int i = 0; i < nColumns; i++) {
      // Build the columns.
      pnlSectionColumns[i] = new JPanel();
      pnlSectionColumns[i]
          .setLayout(new BoxLayout(pnlSectionColumns[i], BoxLayout.Y_AXIS));
      pnlSections.add(pnlSectionColumns[i]);
      if (i < nColumns - 1) {
        pnlSections.add(Box.createRigidArea(FixedDim.x5_y0));
        pnlSections.add(new JSeparator(SwingConstants.VERTICAL));
        pnlSections.add(Box.createRigidArea(FixedDim.x5_y0));
      }
      // Add the checkboxes and tempoarily store the sections in this column.
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
      // Put padding between the chckboxes and the section panels.
      if (extraRow == 0) {
        pnlSectionColumns[i].add(Box.createRigidArea(FixedDim.x0_y20));
      }
      else {
        pnlSectionColumns[i].add(Box.createRigidArea(FixedDim.x0_y5));
      }
      // Add the section panels.
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
    cbShowOnlyIncluded.addActionListener(listener);
    btnCloseAll.addActionListener(listener);
    btnCancel.addActionListener(listener);
    btnSave.addActionListener(listener);
    IncludeListener includeListener = new IncludeListener(this);
    ExcludeListener excludeListener = new ExcludeListener(this);
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      cbInclude[i].addActionListener(includeListener);
      cbExclude[i].addActionListener(excludeListener);
    }
  }

  private void setTooltips() {
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      String fileName = DirectiveFileType.getInstance(i)
          .getLocalFile(manager, AxisID.ONLY).getName();
      cbInclude[i].setToolTipText("Include directives found in " + fileName);
      cbExclude[i].setToolTipText("Exclude directives found in " + fileName);
    }
    cbShowUnchanged.setToolTipText("Show directives whose values have not changed.");
    cbShowHidden
        .setToolTipText("Show directives that are usually not included in a directive file.");
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

  public String getSaveFileAbsPath() {
    JFileChooser chooser = UIHarness.INSTANCE.getFileChooser();
    if (chooser == null) {
      return null;
    }
    if (lastFileChooserLocation != null) {
      chooser.setCurrentDirectory(lastFileChooserLocation);
    }
    else {
      chooser.setCurrentDirectory(builder.getDefaultSaveLocation());
    }
    chooser.setDialogTitle("Save As");
    chooser.setFileFilter(new AutodocFilter());
    if (type == DirectiveFileType.USER) {
      chooser.setFileHidingEnabled(false);
    }
    if (chooser.showOpenDialog(pnlRoot) == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      if (file != null) {
        return file.getAbsolutePath();
      }
    }
    lastFileChooserLocation = chooser.getCurrentDirectory();
    return null;
  }

  public List<String> getComments() {
    List<String> comments = new ArrayList<String>();
    comments.add(ltfSource.getLabel() + " " + ltfSource.getText());
    comments.add(ltfDatasetTimestamp.getLabel() + " " + ltfDatasetTimestamp.getText());
    return comments;
  }

  public List<String> getDroppedDirectives() {
    return builder.getDroppedDirectives();
  }

  public void setFileTimestamp(final Date date) {
    String[] dateArray = date.toString().split("\\s+");
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < dateArray.length - 2; i++) {
      buffer.append(dateArray[i] + " ");
    }
    ltfFileTimestamp.setText(buffer.toString().trim());
  }

  public List<Directive> getIncludeDirectiveList() {
    List<Directive> directiveList = new ArrayList<Directive>();
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    while (iterator.hasNext()) {
      Collection<Directive> directives = iterator.next().getIncludeDirectiveList();
      if (directives != null) {
        directiveList.addAll(directives);
      }
    }
    return directiveList;
  }

  public void checkpoint() {
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    while (iterator.hasNext()) {
      iterator.next().checkpoint();
    }
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

  public List<DirectiveSectionPanel> getSectionList() {
    return sectionArray;
  }

  private void action(final String actionCommand) {
    if (btnCloseAll.getActionCommand().equals(actionCommand)) {
      Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
      while (iterator.hasNext()) {
        iterator.next().close();
      }
      UIHarness.INSTANCE.pack(manager);
    }
    else if (btnCancel.getActionCommand().equals(actionCommand)) {
      UIHarness.INSTANCE.cancel(manager);
    }
    else if (btnSave.getActionCommand().equals(actionCommand)) {
      UIHarness.INSTANCE.save(manager, AxisID.ONLY);
    }
    else {
      if (cbShowOnlyIncluded.getActionCommand().equals(actionCommand)) {
        boolean enable = !cbShowOnlyIncluded.isSelected();
        cbShowUnchanged.setEnabled(enable);
        cbShowHidden.setEnabled(enable && type != DirectiveFileType.BATCH);
      }
      msgControlChanged(false);
    }
  }

  public boolean isShowOnlyIncluded() {
    return cbShowOnlyIncluded.isSelected();
  }

  public boolean isDifferentFromCheckpoint(final boolean checkInclude) {
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    while (iterator.hasNext()) {
      if (iterator.next().isDifferentFromCheckpoint(checkInclude)) {
        return true;
      }
    }
    return false;
  }

  private void msgControlChanged(final boolean includeChange) {
    boolean showOnlyIncluded = cbShowOnlyIncluded.isSelected();
    Iterator<DirectiveSectionPanel> iterator = sectionArray.iterator();
    while (iterator.hasNext()) {
      iterator.next()
          .msgControlChanged(includeChange, showOnlyIncluded, showOnlyIncluded);
    }
    UIHarness.INSTANCE.pack(manager);
  }

  private void includeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      if (cbInclude[index].isSelected() || cbExclude[index].isSelected()) {
        cbExclude[index].setSelected(false);
      }
    }
    msgControlChanged(true);
  }

  private void excludeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      if (cbExclude[index].isSelected() || cbInclude[index].isSelected()) {
        cbInclude[index].setSelected(false);
      }
    }
    msgControlChanged(true);
  }

  public void expand(final ExpandButton button) {
    if (phControl.equalsOpenClose(button)) {
      pnlControlBody.setVisible(button.isExpanded());
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
      dialog.action(event.getActionCommand());
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
