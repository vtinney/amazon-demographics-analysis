## 🔄 Workflow for Collaboration

### For the Intern:
1. **Open notebook** from GitHub in Colab
2. **Work in Colab** with auto-save to Google Drive
3. **Save to GitHub** when done: File → Save a copy in GitHub
4. **Commit message**: Describe what was accomplished

### For You (Supervisor):
1. **Monitor progress** via GitHub commits
2. **Review outputs** in the repository
3. **Provide feedback** via GitHub issues or comments
4. **Track deliverables** using the checkboxes in each notebook

---

## 🚨 Important Setup Notes

### GitHub Settings
- **Enable GitHub Pages** (optional): Settings → Pages → Source: Deploy from branch
- **Branch protection** (optional): Protect main branch from direct pushes
- **Issues enabled**: For tracking problems and feedback

### Colab Integration
- **Colab auto-saves** to Google Drive by default
- **Manual GitHub saves** preserve work in the repository  
- **Private repos** require GitHub authentication in Colab

### File Size Limits
- **GitHub limit**: 100MB per file
- **Large data files**: Use .gitignore to exclude, store elsewhere
- **Outputs**: HTML maps and reports can be large

---

## ✅ Verification Checklist

Before sharing with your intern:

- [ ] Repository created with correct name
- [ ] Folder structure matches specification
- [ ] 01_environment_setup.ipynb uploaded and works in Colab
- [ ] 02_census_data.ipynb uploaded and accessible
- [ ] README.md updated with correct GitHub username
- [ ] Colab badges point to correct URLs
- [ ] .gitignore prevents large file uploads
- [ ] Helper functions uploaded to utils/ folder
- [ ] Intern has access to repository

---

## 🎯 Quick Test

To verify everything works:

1. **Click your own Colab badge** in the README
2. **Run the setup notebook** to ensure packages install
3. **Try saving to GitHub** from Colab
4. **Check that changes appear** in your repository
