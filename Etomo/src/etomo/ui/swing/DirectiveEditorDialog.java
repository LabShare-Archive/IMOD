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
import javax.swing.JTextField;
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
  private final JPanel pnlIncludeSettings = new JPanel();
  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlSourceBody = new JPanel();
  private final List<DirectiveSectionPanel> directiveSectionArray = new ArrayList<DirectiveSectionPanel>();

  private final BaseManager manager;
  private final PanelHeader phConfig;
  private final PanelHeader phSource;
  private final DirectiveTool tool;
  private final boolean[] fileTypeExists;

  private int changedIndex = -1;

  private DirectiveEditorDialog(final BaseManager manager, final DirectiveFileType type,
      final DirectiveEditorBuilder builder) {
    this.manager = manager;
    phConfig = PanelHeader.getMoreLessInstance("Configure Editor", this,
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
    JScrollPane scrollPane = new JScrollPane(pnlRoot);
    JPanel pnlConfig = new JPanel();
    JPanel pnlShow = new JPanel();
    JPanel pnlSource = new JPanel();
    JPanel pnlIncludeCheckboxes = new JPanel();
    JPanel pnlShowGlue = new JPanel();
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
    }
    // init
    ltfSource.setEditable(false);
    ltfSource.setText(sourceStatus);
    ltfTimestamp.setEditable(false);
    ltfTimestamp.setText(saveTimestamp);
    // root panel
    ToolTipManager.sharedInstance().registerComponent(pnlRoot);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlSource);
    pnlRoot.add(pnlConfig);
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
    pnlIncludeCheckboxes.setLayout(new GridLayout(5, 3, 0, 0));
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
    // section panels
    Iterator<DirectiveDescrSection> iterator = builder.getSectionArray().iterator();
    DirectiveDescrSection descrSection;
    final DirectiveMap directiveMap = builder.getDirectiveMap();
    while (iterator.hasNext()) {
      descrSection = iterator.next();
      DirectiveSectionPanel section = DirectiveSectionPanel.getInstance(manager,
          descrSection, directiveMap, sourceAxisType, tool);
      pnlRoot.add(section.getComponent());
      directiveSectionArray.add(section);
    }
    descrSection = builder.getOtherSection();
    if (descrSection != null) {
      DirectiveSectionPanel section = DirectiveSectionPanel.getInstance(manager,
          descrSection, directiveMap, sourceAxisType, tool);
      pnlRoot.add(section.getComponent());
      directiveSectionArray.add(section);
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

  public int getChangedIndex() {
    return changedIndex;
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

  /**
   * Updates whether a directives are being shown or whether their include checkboxes are
   * checked.  If null is passed, function will update include checkboxes.
   * @param actionCommand
   */
  private void action(final String actionCommand) {
    // Update directive display settings.
    if (actionCommand != null) {
      changedIndex = -1;
    }
    // update sections displays
    Iterator<DirectiveSectionPanel> iterator = directiveSectionArray.iterator();
    while (iterator.hasNext()) {
      // Update include if action was called from include/excludeAction (which means that
      // the action command parameter is null).
      iterator.next().updateDisplay(actionCommand == null);
    }
  }

  private void includeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      changedIndex = index;
      if (cbInclude[index].isSelected() || cbExclude[index].isSelected()) {
        cbExclude[index].setSelected(false);
      }
    }
    action(null);
  }

  private void excludeAction(final String actionCommand) {
    DirectiveFileType directiveFileType = DirectiveFileType.getInstance(actionCommand);
    if (directiveFileType != null) {
      int index = directiveFileType.getIndex();
      changedIndex = index;
      if (cbExclude[index].isSelected() || cbInclude[index].isSelected()) {
        cbInclude[index].setSelected(false);
      }
    }
    action(null);
  }

  public void expand(final ExpandButton button) {
    if (phConfig.equalsOpenClose(button)) {
      pnlConfigBody.setVisible(button.isExpanded());
    }
    else if (phConfig.equalsMoreLess(button)) {
      pnlIncludeSettings.setVisible(button.isExpanded());
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
