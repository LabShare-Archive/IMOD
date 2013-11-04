package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.type.Transform;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
final class TransformChooserPanel {
  public static final String rcsid = "$Id:$";

  private static final String SEARCH_LABEL = "Search For:";

  private final JPanel pnlRoot = new JPanel();
  private final ButtonGroup bgTransform = new ButtonGroup();
  private final RadioButton rbFullLinearTransformation = new RadioButton(
      "Full linear transformation", bgTransform);
  private final RadioButton rbRotationTranslationMagnification = new RadioButton(
      "Rotation/translation/magnification", bgTransform);
  private final RadioButton rbRotationTranslation = new RadioButton(
      "Rotation/translation", bgTransform);

  private final RadioButton rbTranslation;
  private final CheckBox cbSearch;

  private TransformChooserPanel(final boolean allowTranslationsAlone,
      final boolean optionalSearch) {
    if (allowTranslationsAlone) {
      rbTranslation = new RadioButton("Translation", bgTransform);
    }
    else {
      rbTranslation = null;
    }
    if (optionalSearch) {
      cbSearch = new CheckBox(SEARCH_LABEL);
    }
    else {
      cbSearch = null;
    }
  }

  static TransformChooserPanel getJoinModelInstance() {
    TransformChooserPanel instance = new TransformChooserPanel(true, false);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  static TransformChooserPanel getJoinAlignInstance() {
    TransformChooserPanel instance = new TransformChooserPanel(false, false);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  static TransformChooserPanel getSerialSectionsInstance() {
    TransformChooserPanel instance = new TransformChooserPanel(false, true);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    setTransform(null);
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    JPanel pnlChooser = new JPanel();
    pnlRoot.add(pnlChooser);
    pnlRoot.add(Box.createHorizontalGlue());
    // choooser panel
    pnlChooser.setLayout(new BoxLayout(pnlChooser, BoxLayout.Y_AXIS));
    pnlChooser.setAlignmentX(Component.CENTER_ALIGNMENT);
    if (cbSearch != null) {
      pnlChooser.add(cbSearch);
    }
    else {
      pnlChooser.add(new JLabel(SEARCH_LABEL));
    }
    pnlChooser.add(rbFullLinearTransformation.getComponent());
    pnlChooser.add(rbRotationTranslationMagnification.getComponent());
    pnlChooser.add(rbRotationTranslation.getComponent());
    if (rbTranslation != null) {
      pnlChooser.add(rbTranslation.getComponent());
    }
  }

  private void addListeners() {
    if (cbSearch != null) {
      cbSearch.addActionListener(new TransformChooserListener(this));
    }
  }

  String getSearchActionCommand() {
    if (cbSearch != null) {
      return cbSearch.getActionCommand();
    }
    return null;
  }

  void addSearchListener(final ActionListener listener) {
    cbSearch.addActionListener(listener);
  }

  public Component getComponent() {
    return pnlRoot;
  }

  private void action() {
    updateDisplay();
  }

  private void updateDisplay() {
    boolean enable = cbSearch.isSelected();

    rbFullLinearTransformation.setEnabled(enable);
    rbRotationTranslationMagnification.setEnabled(enable);
    rbRotationTranslation.setEnabled(enable);
    if (rbTranslation != null) {
      rbTranslation.setEnabled(enable);
    }
  }

  private void setTooltips() {
    if (cbSearch != null) {
      cbSearch
          .setToolTipText("Use iterative search to find best transformation for aligning images.");
    }
    rbFullLinearTransformation
        .setToolTipText("Use rotation, translation, magnification, and stretching to align images.");
    rbRotationTranslationMagnification
        .setToolTipText("Use translation, rotation, and magnification to align images.");
    rbRotationTranslation.setToolTipText("Use translation and rotation to align images.");
    if (rbTranslation != null) {
      rbTranslation.setToolTipText("Use translation to align images.");
    }
  }

  boolean isSearch() {
    if (cbSearch == null) {
      // Search is always on
      return true;
    }
    return cbSearch.isSelected();
  }

  Transform getTransform() {
    if (cbSearch != null && !cbSearch.isSelected()) {
      return Transform.SKIP_SEARCH;
    }
    if (rbFullLinearTransformation.isSelected()) {
      return Transform.FULL_LINEAR_TRANSFORMATION;
    }
    if (rbRotationTranslationMagnification.isSelected()) {
      return Transform.ROTATION_TRANSLATION_MAGNIFICATION;
    }
    if (rbRotationTranslation.isSelected()) {
      return Transform.ROTATION_TRANSLATION;
    }
    if (rbTranslation.isSelected()) {
      return Transform.TRANSLATION;
    }
    return Transform.DEFAULT;
  }

  void setTransform(Transform transform) {
    if (cbSearch != null) {
      if (transform == Transform.SKIP_SEARCH) {
        cbSearch.setSelected(false);
      }
      else {
        cbSearch.setSelected(true);
      }
      updateDisplay();
    }
    if (transform != Transform.SKIP_SEARCH) {
      if (transform == null) {
        transform = Transform.DEFAULT;
      }
      if (transform == Transform.FULL_LINEAR_TRANSFORMATION) {
        rbFullLinearTransformation.setSelected(true);
      }
      else if (transform == Transform.ROTATION_TRANSLATION_MAGNIFICATION) {
        rbRotationTranslationMagnification.setSelected(true);
      }
      else if (transform == Transform.ROTATION_TRANSLATION) {
        rbRotationTranslation.setSelected(true);
      }
      else if (transform == Transform.TRANSLATION) {
        rbTranslation.setSelected(true);
      }
    }
  }

  private static final class TransformChooserListener implements ActionListener {
    private final TransformChooserPanel panel;

    private TransformChooserListener(final TransformChooserPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action();
    }
  }
}
