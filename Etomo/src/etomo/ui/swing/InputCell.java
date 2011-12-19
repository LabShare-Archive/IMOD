package etomo.ui.swing;

import java.awt.Component;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;
import javax.swing.plaf.ColorUIResource;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description:  Uses lazy construction.  The inheritor should add the
 * following calls to its 
 * constructor or initializer:  setBackground, setForeground(), setFont()
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
abstract class InputCell implements Cell {
  public static final String rcsid = "$Id$";

  private boolean editable = true;
  private boolean highlight = false;
  private boolean warning = false;
  private boolean error = false;
  private Font plainFont = null;
  private Font italicFont = null;
  private JPanel jpanelContainer = null;
  private boolean initialized = false;
  private String tableHeader = null;
  private HeaderCell rowHeader = null, columnHeader = null;

  abstract Component getComponent();

  abstract UITestFieldType getFieldType();

  abstract int getWidth();

  public abstract void setEnabled(boolean enabled);

  abstract void setToolTipText(String toolTipText);

  public  void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    layout.setConstraints(getComponent(), constraints);
    panel.add(getComponent());
    jpanelContainer = panel;
  }

  final void remove() {
    if (jpanelContainer != null) {
      jpanelContainer.remove(getComponent());
      jpanelContainer = null;
    }
  }

  void setEditable(boolean editable) {
    this.editable = editable;
    getComponent().setEnabled(editable);
    setBackground();
  }

  boolean isEditable() {
    return getComponent().isEnabled();
  }

  final void setHighlight(boolean highlight) {
    this.highlight = highlight;
    setBackground();
  }

  public final void setWarning(boolean warning) {
    //if switching from error to warning, turn off error first
    if (warning && error) {
      this.warning = false;//prevent recursion
      setError(false);
    }
    this.warning = warning;
    setBackground();
  }

  final void setWarning(boolean warning, String tooltip) {
    setWarning(warning);
    setToolTipText(tooltip);
  }

  final void setError(boolean error, String tooltip) {
    setError(error);
    setToolTipText(tooltip);
  }

  final void setError(boolean error) {
    //if switching from warning to error, turn off warning first
    if (error && warning) {
      this.error = false;//prevent recursion
      setWarning(false);
    }
    this.error = error;
    setBackground();
  }

  void setBackground() {
    if (highlight) {
      if (editable) {
        setBackground(Colors.HIGHLIGHT_BACKGROUND);
      }
      else {
        setBackground(Colors.HIGHLIGHT_BACKGROUND_NOT_EDITABLE);
      }
    }
    else if (warning) {
      if (editable) {
        setBackground(Colors.WARNING_BACKGROUND);
      }
      else {
        setBackground(Colors.WARNING_BACKGROUND_NOT_EDITABLE);
      }
    }
    else if (error) {
      if (editable) {
        setBackground(Colors.CELL_ERROR_BACKGROUND);
      }
      else {
        setBackground(Colors.CELL_ERROR_BACKGROUND_NOT_EDITABLE);
      }
    }
    else if (editable) {
      setBackground(Colors.BACKGROUND);
    }
    else {
      setBackground(Colors.getCellNotEditableBackground());
    }
  }

  void setFont() {
    plainFont = getComponent().getFont();
    italicFont = new Font(plainFont.getFontName(), Font.ITALIC, plainFont.getSize());
  }

  private void setBackground(ColorUIResource color) {
    getComponent().setBackground(color);
  }

  void setHeaders(String tableHeader, HeaderCell rowHeader, HeaderCell columnHeader) {
    this.tableHeader = tableHeader;
    this.rowHeader = rowHeader;
    this.columnHeader = columnHeader;
    setName();
    rowHeader.addChild(this);
    columnHeader.addChild(this);
  }

  /**
   * Message from row header or column header that their label has changed.
   */
  public void msgLabelChanged() {
    setName();
  }
  
  String convertLabelToName() {
    return Utilities.convertLabelToName(tableHeader, rowHeader != null ? rowHeader
        .getText() : null, columnHeader != null ? columnHeader.getText() : null);
  }

  /**
   * Build the name out of table header, row header, and column header.
   */
   void setName() {
    String name = convertLabelToName();
    getComponent().setName(
        getFieldType().toString() + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getComponent().getName() + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.15  2010/01/13 21:55:43  sueh
 * <p> bug# 1298 Fixed highlight colors.
 * <p>
 * <p> Revision 1.14  2009/11/20 17:12:57  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.13  2009/10/15 23:32:56  sueh
 * <p> bug# 1274 Made msgLabelChanged public so it can be added to Cell.
 * <p>
 * <p> Revision 1.12  2009/01/20 20:11:41  sueh
 * <p> bug# 1102 Added functionality to self-name; added columnHeader,
 * <p> rowHeader, tableHeader, getFieldType, msgLabelChanged, setHeaders,
 * <p> and setName.
 * <p>
 * <p> Revision 1.11  2008/08/21 00:07:42  sueh
 * <p> bug# 1132 Added isEditable.
 * <p>
 * <p> Revision 1.10  2007/09/27 20:51:02  sueh
 * <p> bug# 1044 Made setWarning public.
 * <p>
 * <p> Revision 1.9  2007/04/02 21:49:50  sueh
 * <p> bug# 964 Implementing Cell interface.
 * <p>
 * <p> Revision 1.8  2007/03/27 19:31:11  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.7  2007/03/01 01:38:21  sueh
 * <p> bug# 964 Made InputCell colors constant and moved them to Colors.  Added
 * <p> setExpandableValues, getContractedValue, and getExpandedValue.
 * <p>
 * <p> Revision 1.6  2007/02/05 23:38:41  sueh
 * <p> bug# 962 Moved color info to UIUtilities.
 * <p>
 * <p> Revision 1.5  2006/10/17 20:20:22  sueh
 * <p> bug# 919  Made background and warningBackground static and available to
 * <p> package.  Made subtract available to package.
 * <p>
 * <p> Revision 1.4  2006/10/16 22:51:16  sueh
 * <p> bug# 919  Added abstract setToolTipText().
 * <p>
 * <p> Revision 1.3  2005/08/04 20:11:15  sueh
 * <p> bug# 532 Fixed setWarning and setError.
 * <p>
 * <p> Revision 1.2  2005/07/19 22:32:40  sueh
 * <p> bug# 532 changing the look of inUse == false to greyed out text.
 * <p> Changing the look of error == true to red background.  Added warning
 * <p> boolean.  Setting color internally instead of relying on UI color.
 * <p>
 * <p> Revision 1.1  2005/07/01 21:20:08  sueh
 * <p> bug# 619 Pulled an ancestor class (InputCell) out of FieldCell because we
 * <p> need several types of input cells.  InputCell handles states and colors
 * <p> but not display fields.
 * <p> </p>
 */
