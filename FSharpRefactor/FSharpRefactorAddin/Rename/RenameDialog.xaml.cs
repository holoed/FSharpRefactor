using System;
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
