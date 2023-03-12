# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'mainwindow.ui'
#
# Created: Tue Aug 29 21:13:23 2017
#      by: PyQt4 UI code generator 4.10.2
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName(_fromUtf8("MainWindow"))
        MainWindow.resize(800, 600)
        self.centralwidget = QtGui.QWidget(MainWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(MainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 26))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        self.menuMap = QtGui.QMenu(self.menubar)
        self.menuMap.setObjectName(_fromUtf8("menuMap"))
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(MainWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        MainWindow.setStatusBar(self.statusbar)
        self.actionZoom_In = QtGui.QAction(MainWindow)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/qgis-qgs.ico")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionZoom_In.setIcon(icon)
        self.actionZoom_In.setObjectName(_fromUtf8("actionZoom_In"))
        self.actionZoom_Out = QtGui.QAction(MainWindow)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/qgis-qlr.ico")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionZoom_Out.setIcon(icon1)
        self.actionZoom_Out.setObjectName(_fromUtf8("actionZoom_Out"))
        self.actionZoom_Full = QtGui.QAction(MainWindow)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/qgis.ico")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionZoom_Full.setIcon(icon2)
        self.actionZoom_Full.setObjectName(_fromUtf8("actionZoom_Full"))
        self.actionAdd_Layer = QtGui.QAction(MainWindow)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/browser.ico")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAdd_Layer.setIcon(icon3)
        self.actionAdd_Layer.setObjectName(_fromUtf8("actionAdd_Layer"))
        self.menuMap.addAction(self.actionZoom_In)
        self.menuMap.addAction(self.actionZoom_Out)
        self.menuMap.addAction(self.actionZoom_Full)
        self.menuMap.addAction(self.actionAdd_Layer)
        self.menubar.addAction(self.menuMap.menuAction())

        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(_translate("MainWindow", "MainWindow", None))
        self.menuMap.setTitle(_translate("MainWindow", "Map", None))
        self.actionZoom_In.setText(_translate("MainWindow", "pActionZoomIn", None))
        self.actionZoom_Out.setText(_translate("MainWindow", "mpActionZoomOut", None))
        self.actionZoom_Out.setToolTip(_translate("MainWindow", "mpActionZoomOut", None))
        self.actionZoom_Full.setText(_translate("MainWindow", "mpActionZoomFull", None))
        self.actionZoom_Full.setToolTip(_translate("MainWindow", "mpActionZoomFull", None))
        self.actionAdd_Layer.setText(_translate("MainWindow", "mpActionAddLayer", None))
        self.actionAdd_Layer.setToolTip(_translate("MainWindow", "mpActionAddLayer", None))

import resources_rc
