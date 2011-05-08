// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

using System.Windows;
using System.Windows.Controls;

namespace FSharpRefactorAddin.Rename
{
    /// <summary>
    /// Interaction logic for RenameDialog.xaml
    /// </summary>
    public partial class RenameDialog : Window
    {
        public RenameDialog()
        {
            InitializeComponent();

            textBox1.TextChanged += HandleTextChanged;
        }

        private void HandleTextChanged(object sender, TextChangedEventArgs e)
        {
            SelectAll(sender);
            textBox1.TextChanged -= HandleTextChanged;
        }

        private static void SelectAll(object sender)
        {
            if (sender is TextBox)
                (sender as TextBox).SelectAll();
        }

        private void HandleOk(object sender, RoutedEventArgs e)
        {
            DialogResult = true;
        }

        private void HandleCancel(object sender, RoutedEventArgs e)
        {
            DialogResult = false;
        }
    }
}
