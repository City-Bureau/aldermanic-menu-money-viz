# Chicago Aldermanic Menu Money Viz

Data visualizations for Chicago Aldermanic menu money

## Setup

```bash
npm install
npm start
```

## Notes

### Layout

- Donut chart in the middle
    - Add tooltip of amounts on hover
    - Potentially a list of items on the side, showing amounts
    - Show colors next to it for legend, use CSS prop to keep consistent width
- Map next to it that includes all of the wards, council members
    - Disable drag pan on map on mobile, but allow zoom with pinch
- Clicking on a ward updates the select and chooses the council member
- Descriptive text about ward above as well
