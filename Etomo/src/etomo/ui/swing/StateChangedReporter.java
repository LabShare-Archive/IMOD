package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;

/**
* <p>Description: This class listens for changes in its sources, decides if these changes
* are important and, if they are, reports to its listeners.  The leadSource member variable
* is used to help decide whether the changes are important.</p>
* <h4>Current use of this class</h4>
* <p>The sources are fields that have been checkpointed.  The leadSource is the dialog
* containing the fields.  A true state is a difference between the checkpoint and the
* current value.  A false state is not difference between the checkpoint and the current
* value.  An important field state change is when the field value first diverges
* from the checkpointed value.  But this change is unimportant at the dialog level if
* another of dialog's checkpointed fields have already diverged.  So the field that fired
* the action is consulted and then the dialog is consulted if necessary, before the event
* is fired to the listeners.  The listeners dialog(s) that need to change when a
* checkpointed field first diverges from the checkpoint value.  They also need to change
* when all fields first become identical to the checkpoint values, either from a user
* action or a new checkpoint.</p>
* <p>The purpose of this algorithm is to reduce unnecessary calls to the leadSource and
* the listeners.  Unnecessary calls probably can't be eliminated completely because of the
* speed that changes happen.</p>
* 
* <p>Copyright: Copyright 2011</p>
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
final class StateChangedReporter implements ActionListener, DocumentListener {
  public static final String rcsid = "$Id$";

  private final List<StateChangedListener> listeners = new ArrayList<StateChangedListener>();
  private final List<SourceState> sourceStates = new Vector<SourceState>();
  private final TiltPanel leadSource;

  StateChangedReporter(final TiltPanel leadSource) {
    this.leadSource = leadSource;
  }

  /**
   * Add a source.  Add this instance as an action listener to the source.  A source will
   * be added only once.
   * @param source
   */
  void addSource(final StateChangeActionSource source) {
    if (sourceStates.indexOf(source) == -1) {
      source.addActionListener(this);
      addSourceState(source);
    }
  }

  /**
   * Add a source.  Add this instance as a key listener to the source.  A source will be
   * added only once.
   * @param source
   */
  void addSource(final StateChangeDocumentSource source) {
    if (sourceStates.indexOf(source) == -1) {
      source.addDocumentListener(this);
      addSourceState(source);
    }
  }

  /**
   * Add a source.  Add this instance as an action listener to the source.  A source will
   * be added only once.
   * @param source
   */
  void addSource(final StateChangeActionAndDocumentSource source) {
    if (sourceStates.indexOf(source) == -1) {
      source.addActionListener(this);
      source.addDocumentListener(this);
      addSourceState(source);
    }
  }

  private void addSourceState(final StateChangeSource source) {
    source.setReporter(this);
    sourceStates.add(new SourceState(source));
  }

  void msgCheckpointed(final StateChangeSource source) {
    int index = sourceStates.indexOf(source);
    if (index == -1) {
      return;
    }
    SourceState sourceState = sourceStates.get(index);
    if (sourceState != null) {
      sourceState.msgCheckpointed();
    }
  }

  /**
   * If the source of the event has a new state, check whether the leadSource's state has
   * changed.  Fire the event if both have changed.
   */
  public void actionPerformed(final ActionEvent event) {
    int index = -1;
    for (int i = 0; i < sourceStates.size(); i++) {
      if (sourceStates.get(i).equals(event.getSource())) {
        index = i;
        break;
      }
    }
    if (index != -1) {
      checkStateChanged(index);
    }
  }

  public void changedUpdate(final DocumentEvent event) {
    documentUpdate(event);
  }

  public void insertUpdate(final DocumentEvent event) {
    documentUpdate(event);
  }

  public void removeUpdate(final DocumentEvent event) {
    documentUpdate(event);
  }

  private void documentUpdate(final DocumentEvent event) {
    int index = -1;
    for (int i = 0; i < sourceStates.size(); i++) {
      if (sourceStates.get(i).equals(event.getDocument())) {
        index = i;
        break;
      }
    }
    if (index != -1) {
      checkStateChanged(index);
    }
  }

  private void checkStateChanged(final int index) {
    SourceState sourceState = sourceStates.get(index);
    if (sourceState != null && sourceState.isStateChanged()
        && leadSource.isStateChanged()) {
      fireStateChangedEvent();
    }
  }

  void addStateChangedListener(final StateChangedListener listener) {
    listeners.add(listener);
  }

  /**
   * If there are listeners, fire a state change event to each listener.
   */
  private synchronized void fireStateChangedEvent() {
    StateChangedEvent event = new StateChangedEvent(leadSource);
    Iterator<StateChangedListener> i = listeners.iterator();
    while (i.hasNext()) {
      i.next().stateChanged(event);
    }
  }

  /**
   * Contains a source and the most recently updated state.
   * @author sueh
   */
  private static final class SourceState {
    private final StateChangeSource source;
    private boolean state = false;

    private SourceState(StateChangeSource source) {
      this.source = source;
      state = source.getState();
    }

    public String toString() {
      return "[source:" + source + ",state:" + state + "]";
    }

    /**
     * Two objects are equal if source is the same instance.  This is used to prevent a
     * source from being stored in multiple states in the same collection.
     */
    public boolean equals(Object object) {
      if (object == source) {
        return true;
      }
      if (object instanceof SourceState) {
        return ((SourceState) object).source == source;
      }
      return source.equals(object);
    }

    /**
     * Two objects are equal if source is the same instance.  This is used to prevent a
     * source from being stored in multiple states in the same collection.
     */
    public boolean equals(Document document) {
      return source.equals(document);
    }

    private void msgCheckpointed() {
      state = false;
    }

    private StateChangeSource getSource() {
      return source;
    }

    private boolean isStateChanged() {
      boolean sourceState = source.getState();
      if (state != sourceState) {
        state = sourceState;
        return true;
      }
      return false;
    }

    private boolean getState() {
      return state;
    }
  }
}
