using System.Windows;

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
