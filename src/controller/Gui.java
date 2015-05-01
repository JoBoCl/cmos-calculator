package controller;

import com.mxgraph.layout.mxFastOrganicLayout;
import com.mxgraph.layout.mxGraphLayout;
import com.mxgraph.layout.mxParallelEdgeLayout;
import com.mxgraph.model.mxGraphModel;
import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.util.mxConstants;
import com.mxgraph.util.mxPoint;
import com.mxgraph.view.mxGraph;
import com.mxgraph.view.mxGraphView;
import com.mxgraph.view.mxStylesheet;
import helper.CMOSLayout;
import helper.Parser;
import model.Node;
import model.Result;
import model.Variable;
import scala.Option;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.DefaultEntry;
import view.DrawCircuit;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by joshua on 08/04/15.
 */
public class Gui {
    /**
     * Handler for the whole graph.
     */
    public mxGraph graph;
    /**
     * Handler for the graphic component.
     */
    public mxGraphComponent graphComponent = null;
    /**
     * A handler for the graph's layout.
     */
    public mxGraphLayout layout;
    private JPanel mainPanel;
    private JPanel visPanel;
    private JPanel inputPanel;
    private JTextField textInput;
    private JButton goButton;
    private JCheckBox outputCheckBox;
    private JPanel variableHolder;
    private HashMap<String, JCheckBox> nameBoxMap = new HashMap<String, JCheckBox>();
    private DrawCircuit drawCircuit;

    public Gui () {
        goButton.addActionListener(new ActionListener() {
            @Override public void actionPerformed (ActionEvent actionEvent) {
                // Clear checkboxes
                variableHolder.removeAll();
                variableHolder.add(outputCheckBox);
                Variable.clear();

                Option<Node> parseResult = Parser.variableParser(textInput.getText());

                if (parseResult.isEmpty()) {
                    JOptionPane.showMessageDialog(mainPanel, "Could not parse expression", "Error",
                                                         JOptionPane.ERROR_MESSAGE);
                } else {
                    final Node result = parseResult.get();
                    CMOSLayout.layout(result);
                    final TreeMap<String, Object> map = Variable.getMap();
                    final Iterator<Tuple2<String, Object>> it = map.iterator();
                    while (it.hasNext()) {
                        Tuple2<String, Object> value = it.next();
                        String variableName = value._1();
                        final JCheckBox variableBox = new JCheckBox(variableName, false);
                        if (!variableName.equals("out")) {
                            variableBox.addItemListener(new ItemListener() {
                                @Override public void itemStateChanged (ItemEvent e) {
                                    Variable.setValue(variableBox.getText(), variableBox.isSelected());
                                    outputCheckBox.setSelected(Result.get().isHigh());
                                    // Clear Graph
                                    if (graph != null) {
                                        ((mxGraphModel) graph.getModel()).clear();
                                    }
                                    drawCircuit.draw();

                                    resizeGraphView(graphComponent);
                                }
                            });
                            variableBox.setSelected(Variable.lookup(variableName));
                            variableHolder.add(variableBox);
                            nameBoxMap.put(variableName, variableBox);
                            variableHolder.validate();
                            variableHolder.repaint();
                        }
                    }
                    visPanel.setSize(400, 300);
                    graphComponent = initGraph();
                    visPanel.add(graphComponent, BorderLayout.CENTER);
                    mainPanel.updateUI();
                    mainPanel.validate();
                    mainPanel.repaint();
                }
                outputCheckBox.setSelected(parseResult.get().get());
            }
        });
    }

    public static void main (String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e1) {
            e1.printStackTrace();
        }

        JFrame frame = new JFrame("CMOS Calculator");
        frame.setContentPane(new Gui().mainPanel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }

    /**
     * This will initialize the graph component to draw the shapes on.
     *
     * @return The graph component which was drawn on.
     */
    public final mxGraphComponent initGraph () {
        mxGraphComponent localGraphComponent = null;
        if (graph == null && graphComponent == null) {
            graph = new mxGraph();
            localGraphComponent = new mxGraphComponent(graph);

            // Allow negative co-ordinates (makes drawing easier as bottom half can be negatively positioned)
            graph.setAllowNegativeCoordinates(true);
            // dangling edges are bad and result in all kinds of nasty things
            graph.setAllowDanglingEdges(false);
            // edge source and target are the same
            graph.setAllowLoops(true);
            // don't need this
            graph.setCellsResizable(false);
            // don't allow movement
            graph.setCellsMovable(false);
            // don't allow new connections
            graph.setConnectableEdges(false);
            // make editing labels more comfortable
            localGraphComponent.setEnterStopsCellEditing(true);
            // antialiasing \o/
            localGraphComponent.setAntiAlias(true);
            // set size
            localGraphComponent.setSize(visPanel.getWidth(), visPanel.getHeight());

            graph.setAutoOrigin(true);

            // define a parallel layout for the edges
            layout = new mxParallelEdgeLayout(graph);

            // change the default edge style to rounded
            mxStylesheet styleSheet = graph.getStylesheet();
            Map<String, Object> edgeStyle = styleSheet.getDefaultEdgeStyle();
            edgeStyle.put(mxConstants.STYLE_ROUNDED, true);
            styleSheet.setDefaultEdgeStyle(edgeStyle);
            graph.setStylesheet(styleSheet);

            drawCircuit = new DrawCircuit(graph);
            drawCircuit.draw();

            resizeGraphView(localGraphComponent);

            //graph.setCollapseToPreferredSize(true);
        }
        if (localGraphComponent == null) {
            return graphComponent;
        } else {
            return localGraphComponent;
        }
    }

    private void resizeGraphView (mxGraphComponent localGraphComponent) {
        mxGraphView view = localGraphComponent.getGraph().getView();
        int compLenH = localGraphComponent.getHeight();
        int viewLenH = (int) view.getGraphBounds().getHeight();
        double scaleH = (double) (compLenH - 1) / viewLenH * view.getScale();

        int compLenW = localGraphComponent.getWidth();
        int viewLenW = (int) view.getGraphBounds().getWidth();

        double scaleW = (double) (compLenW - 1) / viewLenW * view.getScale();

        view.setScale(Math.min(scaleH, scaleW));
    }

    {
        // GUI initializer generated by IntelliJ IDEA GUI Designer
        // >>> IMPORTANT!! <<<
        // DO NOT EDIT OR ADD ANY CODE HERE!
        $$$setupUI$$$();
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer >>> IMPORTANT!! <<< DO NOT edit this method OR call it in your
     * code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$ () {
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout(0, 0));
        visPanel = new JPanel();
        visPanel.setLayout(new BorderLayout(0, 0));
        visPanel.setMinimumSize(new Dimension(400, 300));
        visPanel.setPreferredSize(new Dimension(400, 300));
        mainPanel.add(visPanel, BorderLayout.CENTER);
        visPanel.setBorder(BorderFactory.createTitledBorder("CMOS Output"));
        inputPanel = new JPanel();
        inputPanel.setLayout(new BorderLayout(0, 0));
        mainPanel.add(inputPanel, BorderLayout.WEST);
        final JPanel panel1 = new JPanel();
        panel1.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
        panel1.setEnabled(true);
        inputPanel.add(panel1, BorderLayout.NORTH);
        textInput = new JTextField();
        textInput.setMinimumSize(new Dimension(70, 26));
        textInput.setPreferredSize(new Dimension(100, 26));
        textInput.setText("");
        textInput.setToolTipText("Enter the text to parse here");
        panel1.add(textInput);
        goButton = new JButton();
        goButton.setText("Parse!");
        panel1.add(goButton);
        final JScrollPane scrollPane1 = new JScrollPane();
        inputPanel.add(scrollPane1, BorderLayout.CENTER);
        variableHolder = new JPanel();
        variableHolder.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
        scrollPane1.setViewportView(variableHolder);
        outputCheckBox = new JCheckBox();
        outputCheckBox.setActionCommand("");
        outputCheckBox.setEnabled(false);
        outputCheckBox.setText("out");
        variableHolder.add(outputCheckBox);
    }

    /** @noinspection ALL */
    public JComponent $$$getRootComponent$$$ () {
        return mainPanel;
    }
}
