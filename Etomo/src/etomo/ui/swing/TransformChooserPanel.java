package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;

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
  
  private final RadioButton rbFullLinearTransformation = new RadioButton(
      "Full linear transformation");
  private final RadioButton rbRotationTranslationMagnification = new RadioButton(
      "Rotation/translation/magnification");
  private final RadioButton rbRotationTranslation = new RadioButton(
      "Rotation/translation");
  private final RadioButton rbTranslation = new RadioButton("Translation");

  private JPanel pnlTranslationChooser = null;
  private boolean includeTranslation = false;

  Container getContainer() {
    if (pnlTranslationChooser == null) {
      pnlTranslationChooser = new JPanel();
      pnlTranslationChooser.setLayout(new BoxLayout(pnlTranslationChooser,
          BoxLayout.X_AXIS));
      JPanel ipnlTranslationChooser = new JPanel();
      pnlTranslationChooser.add(ipnlTranslationChooser);
      pnlTranslationChooser.add(Box.createHorizontalGlue());
      ipnlTranslationChooser.setLayout(new BoxLayout(ipnlTranslationChooser,
          BoxLayout.Y_AXIS));
      ipnlTranslationChooser.setAlignmentX(Component.CENTER_ALIGNMENT);
      ipnlTranslationChooser.add(new JLabel("Search For:"));
      ButtonGroup group = new ButtonGroup();
      group.add(rbFullLinearTransformation.getAbstractButton());
      group.add(rbRotationTranslationMagnification.getAbstractButton());
      group.add(rbRotationTranslation.getAbstractButton());
      ipnlTranslationChooser.add(rbFullLinearTransformation.getComponent());
      ipnlTranslationChooser.add(rbRotationTranslationMagnification.getComponent());
      ipnlTranslationChooser.add(rbRotationTranslation.getComponent());
      // set default
      set(null);
      rbFullLinearTransformation
          .setToolTipText("Use rotation, translation, magnification, and stretching to align images.");
      rbRotationTranslationMagnification
          .setToolTipText("Use translation, rotation, and magnification to align images.");
      rbRotationTranslation
          .setToolTipText("Use translation and rotation to align images.");
      if (includeTranslation) {
        group.add(rbTranslation.getAbstractButton());
        ipnlTranslationChooser.add(rbTranslation.getComponent());
        rbTranslation.setToolTipText("Use translation to align images.");
      }
    }
    return pnlTranslationChooser;
  }

  Transform get() {
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

  void set(Transform transform) {
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

  void includeTranslation() {
    includeTranslation = true;
  }
}
